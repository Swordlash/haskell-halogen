// IndexedDB, for Web.Storage.IndexedDB. The GHC JavaScript backend links this
// file and calls these by name; the wasm backend embeds it into its JSFFI
// module (Halogen.JSBits).
//
// IndexedDB answers through events, and the Haskell side waits through one
// awaited import, which on the JavaScript backend may only return nothing. So
// every call is an operation in two halves: a function that describes it and
// returns an object at once (nothing has happened yet), and
// halogen_idb_run, which does it and resolves once it is done, never
// rejecting. The outcome is left on the object, as its result or its error,
// for the Haskell side to read afterwards. A failure is reported the same way
// on both backends that way, rather than as whatever each makes of a rejected
// promise.
//
// Errors are the browser's own (a DOMException, whose name says what went
// wrong) or one of ours, an Error with a name of our choosing:
// UnavailableError when there is no IndexedDB at all, BlockedError when an
// upgrade or a deletion waits on a connection someone else keeps open,
// NotFoundError with the store's name as its message, NotBytesError with the
// key as its message. Web.Storage.IndexedDB turns each name into a Haskell
// failure.

// One open connection per database name, as a promise of it and the version
// it was opened at: opening is the slow part, and a connection can serve any
// number of transactions. A connection is forgotten when it closes, which is
// also what it does when another tab asks for a later version: a connection
// that stayed open would keep that tab's upgrade waiting.
const halogen_idb_connections = new Map();

function halogen_idb_failure(name, message) {
  const error = new Error(message);
  error.name = name;
  return error;
}

// What opening or deleting a database failed with. A page that may not use
// IndexedDB (a sandboxed frame, an opaque origin, storage turned off) has the
// global but is refused with a SecurityError: for the program that is the
// same as having no IndexedDB at all.
function halogen_idb_denied(error) {
  if (error !== null && error !== undefined && error.name === "SecurityError") {
    return halogen_idb_failure("UnavailableError", String(error.message ?? "IndexedDB is not allowed here"));
  }
  return error;
}

function halogen_idb_database(name, version) {
  return { name: name, version: version, stores: [] };
}

function halogen_idb_add_store(database, store) {
  database.stores.push(store);
}

// Forget a connection, unless it has been replaced already.
function halogen_idb_forget(name, promise) {
  const cached = halogen_idb_connections.get(name);
  if (cached !== undefined && cached.promise === promise) {
    halogen_idb_connections.delete(name);
  }
}

// Close the connection to a database, if there is one, and resolve once it
// is closed: a connection still open would block the upgrade or deletion
// that follows.
function halogen_idb_close(name) {
  const cached = halogen_idb_connections.get(name);
  if (cached === undefined) {
    return Promise.resolve();
  }
  halogen_idb_connections.delete(name);
  return cached.promise.then(
    (connection) => connection.close(),
    () => {},
  );
}

// The connection a database descriptor asks for: the cached one when it is at
// the version asked, otherwise a new one, opened once the old one is closed.
function halogen_idb_connect(database) {
  const cached = halogen_idb_connections.get(database.name);
  if (cached !== undefined && cached.version === database.version) {
    return cached.promise;
  }
  const closing = halogen_idb_close(database.name);
  let promise = null;
  const forget = () => halogen_idb_forget(database.name, promise);
  promise = closing.then(() => halogen_idb_open(database, forget));
  halogen_idb_connections.set(database.name, { version: database.version, promise: promise });
  promise.catch(forget);
  return promise;
}

function halogen_idb_open(database, forget) {
  return new Promise((resolve, reject) => {
    if (typeof indexedDB === "undefined") {
      reject(halogen_idb_failure("UnavailableError", "there is no IndexedDB here"));
      return;
    }
    let request;
    try {
      request = indexedDB.open(database.name, database.version);
    } catch (error) {
      reject(halogen_idb_denied(error));
      return;
    }
    // Whether the caller has had its answer. After a blocked upgrade it has,
    // but the request goes on, and succeeds once the other connection closes:
    // the connection it then opens is nobody's, and is closed at once.
    let answered = false;
    request.onupgradeneeded = () => {
      const connection = request.result;
      for (const store of database.stores) {
        if (!connection.objectStoreNames.contains(store)) {
          connection.createObjectStore(store);
        }
      }
    };
    request.onsuccess = () => {
      const connection = request.result;
      if (answered) {
        connection.close();
        return;
      }
      answered = true;
      connection.onversionchange = () => {
        connection.close();
        forget();
      };
      connection.onclose = forget;
      resolve(connection);
    };
    request.onerror = () => {
      if (!answered) {
        answered = true;
        reject(halogen_idb_denied(request.error));
      }
    };
    request.onblocked = () => {
      if (!answered) {
        answered = true;
        reject(halogen_idb_failure("BlockedError", "the database is open elsewhere at an older version"));
      }
    };
  });
}

// A transaction over these stores, after checking the connection has them: a
// store the descriptor names but the database does not have is one added
// without a version bump, and is worth saying so by name.
function halogen_idb_transaction(connection, stores, mode) {
  for (const store of stores) {
    if (!connection.objectStoreNames.contains(store)) {
      throw halogen_idb_failure("NotFoundError", store);
    }
  }
  return connection.transaction(stores, mode);
}

// What a request gives, once it has.
function halogen_idb_request(request) {
  return new Promise((resolve, reject) => {
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => reject(request.error);
  });
}

// Resolves once a transaction has committed, and rejects if it aborts: a
// write's request succeeding does not mean it was kept, its transaction
// committing does.
function halogen_idb_complete(transaction) {
  return new Promise((resolve, reject) => {
    transaction.oncomplete = () => resolve(null);
    transaction.onabort = () => reject(transaction.error ?? halogen_idb_failure("AbortError", "the transaction was aborted"));
  });
}

// A value read back, as bytes. What this library writes is a Uint8Array,
// but a store can hold whatever anything else on the page put into it.
function halogen_idb_bytes(value, key) {
  if (value instanceof Uint8Array) {
    return value;
  }
  if (value instanceof ArrayBuffer) {
    return new Uint8Array(value);
  }
  if (ArrayBuffer.isView(value)) {
    return new Uint8Array(value.buffer, value.byteOffset, value.byteLength);
  }
  throw halogen_idb_failure("NotBytesError", key);
}

function halogen_idb_operation(database, run) {
  return { database: database, run: run, result: null, error: null };
}

function halogen_idb_get(database, store, key) {
  return halogen_idb_operation(database, (connection) => {
    const transaction = halogen_idb_transaction(connection, [store], "readonly");
    // A cursor rather than get(), which answers undefined both when there is
    // no record and when the record is undefined: one that is there and is
    // not bytes is NotBytes here, as it is to halogen_idb_get_all.
    return halogen_idb_request(transaction.objectStore(store).openCursor(key)).then((cursor) =>
      cursor === null ? null : halogen_idb_bytes(cursor.value, key),
    );
  });
}

function halogen_idb_put(database, store, key, bytes) {
  return halogen_idb_operation(database, (connection) => {
    const transaction = halogen_idb_transaction(connection, [store], "readwrite");
    transaction.objectStore(store).put(bytes, key);
    return halogen_idb_complete(transaction);
  });
}

function halogen_idb_delete(database, store, key) {
  return halogen_idb_operation(database, (connection) => {
    const transaction = halogen_idb_transaction(connection, [store], "readwrite");
    transaction.objectStore(store).delete(key);
    return halogen_idb_complete(transaction);
  });
}

function halogen_idb_clear(database, store) {
  return halogen_idb_operation(database, (connection) => {
    const transaction = halogen_idb_transaction(connection, [store], "readwrite");
    transaction.objectStore(store).clear();
    return halogen_idb_complete(transaction);
  });
}

// The keys this library could have written: strings. A key of another kind
// was put there by something else, and could not be asked for by a key of
// text anyway.
function halogen_idb_keys(database, store) {
  return halogen_idb_operation(database, (connection) => {
    const transaction = halogen_idb_transaction(connection, [store], "readonly");
    return halogen_idb_request(transaction.objectStore(store).getAllKeys()).then((keys) =>
      keys.filter((key) => typeof key === "string"),
    );
  });
}

// Every entry, as two arrays, keys and values, in key order. Both are read
// in one transaction, so they are of one moment and line up.
function halogen_idb_get_all(database, store) {
  return halogen_idb_operation(database, (connection) => {
    const transaction = halogen_idb_transaction(connection, [store], "readonly");
    const objectStore = transaction.objectStore(store);
    return Promise.all([
      halogen_idb_request(objectStore.getAllKeys()),
      halogen_idb_request(objectStore.getAll()),
    ]).then(([keys, values]) => {
      const kept = [];
      const bytes = [];
      keys.forEach((key, index) => {
        if (typeof key === "string") {
          kept.push(key);
          bytes.push(halogen_idb_bytes(values[index], key));
        }
      });
      return [kept, bytes];
    });
  });
}

// Writes to several stores in one read-write transaction: all of them are
// kept or none is.
function halogen_idb_batch(database) {
  const operation = halogen_idb_operation(database, (connection) => {
    const stores = [...new Set(operation.writes.map((write) => write.store))];
    if (stores.length === 0) {
      return null;
    }
    const transaction = halogen_idb_transaction(connection, stores, "readwrite");
    for (const write of operation.writes) {
      const objectStore = transaction.objectStore(write.store);
      if (write.bytes === null) {
        objectStore.delete(write.key);
      } else {
        objectStore.put(write.bytes, write.key);
      }
    }
    return halogen_idb_complete(transaction);
  });
  operation.writes = [];
  return operation;
}

function halogen_idb_batch_put(operation, store, key, bytes) {
  operation.writes.push({ store: store, key: key, bytes: bytes });
}

function halogen_idb_batch_delete(operation, store, key) {
  operation.writes.push({ store: store, key: key, bytes: null });
}

function halogen_idb_delete_database(name) {
  return halogen_idb_operation(null, () =>
    halogen_idb_close(name).then(
      () =>
        new Promise((resolve, reject) => {
          if (typeof indexedDB === "undefined") {
            reject(halogen_idb_failure("UnavailableError", "there is no IndexedDB here"));
            return;
          }
          let request;
          try {
            request = indexedDB.deleteDatabase(name);
          } catch (error) {
            reject(halogen_idb_denied(error));
            return;
          }
          request.onsuccess = () => resolve(null);
          request.onerror = () => reject(halogen_idb_denied(request.error));
          request.onblocked = () => reject(halogen_idb_failure("BlockedError", "the database is open elsewhere"));
        }),
    ),
  );
}

// Do an operation, and resolve once it is done, its outcome on the object.
// Calls on one database run their transactions in the order they reach this
// function: each waits for the same connection, and a promise runs what
// waits for it in the order it was asked.
function halogen_idb_run(operation) {
  let work;
  try {
    work = operation.database === null ? operation.run() : halogen_idb_connect(operation.database).then(operation.run);
  } catch (error) {
    work = Promise.reject(error);
  }
  return Promise.resolve(work).then(
    (result) => {
      operation.result = result;
    },
    (error) => {
      operation.error = error ?? halogen_idb_failure("UnknownError", "the operation failed");
    },
  );
}

function halogen_idb_failed(operation) {
  return operation.error !== null;
}

function halogen_idb_error_name(operation) {
  return String(operation.error.name ?? "Error");
}

function halogen_idb_error_message(operation) {
  return String(operation.error.message ?? operation.error);
}

function halogen_idb_result(operation) {
  return operation.result;
}

function halogen_idb_length(array) {
  return array.length;
}

function halogen_idb_index(array, index) {
  return array[index];
}
