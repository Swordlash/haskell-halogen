{-# LANGUAGE OverloadedLabels #-}

-- | The tutorial's addresses: @\/@, the introduction with the list of lessons,
-- @\/\<NN-slug>\/@, a lesson, and @\/playground\/@, the compiler with no
-- lesson around it ("Learn.Playground"), under the site's base path. Each is
-- a directory with an @index.html@ the prerender wrote, so a reload or a link
-- from elsewhere loads that page straight away.
--
-- No lesson can be at @playground@: a lesson's directory, its slug, starts
-- with its two digits ("Learn.Lesson.parseDirectoryName").
module Learn.Route
  ( Route (..)
  , route
  , routes
  , lessonOf
  )
where

import Data.Row ((.+), (.==))
import Halogen.Router.Duplex (RouteDuplex', as, noArgs, path, root, segment, suffix)
import Halogen.Router.Duplex.Generic qualified as Duplex
import Learn.Lesson (Lesson (..))
import Learn.Lessons (lessonBySlug, lessons)
import Protolude

data Route = Home | LessonPage Text | Playground
  deriving stock (Eq, Show, Generic)

-- | A lesson's address is its slug, and only a lesson's slug is one; the
-- playground's is @playground\/@. Any other address is the router's
-- fallback, the introduction.
route :: RouteDuplex' Route
route =
  root
    $ Duplex.sum
    $ #Home
      .== noArgs
      .+ #LessonPage
        .== suffix (as identity known segment) ""
      .+ #Playground
        .== path "playground/" noArgs
  where
    known s = maybe (Left ("no lesson " <> s)) (const (Right s)) (lessonBySlug s)

-- | Every route, the introduction first: the pages the prerender writes.
routes :: [Route]
routes = Home : Playground : map (LessonPage . (.slug)) lessons

lessonOf :: Route -> Maybe Lesson
lessonOf = \case
  Home -> Nothing
  LessonPage s -> lessonBySlug s
  Playground -> Nothing
