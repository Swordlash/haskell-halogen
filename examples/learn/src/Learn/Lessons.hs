{-# LANGUAGE TemplateHaskell #-}

-- | Every lesson, from @examples\/learn\/lessons\/@ in the order of its
-- @index.txt@, embedded at compile time (see "Learn.Lesson" for what a
-- lesson is, and "Learn.Lessons.Embed" for how they are read).
module Learn.Lessons (lessons, lessonBySlug) where

import Learn.Lesson (Lesson (..))
import Learn.Lessons.Embed (embedLessons)
import Protolude

-- | The lessons, in order.
lessons :: [Lesson]
lessons = $(embedLessons "lessons")

lessonBySlug :: Text -> Maybe Lesson
lessonBySlug s = find ((== s) . (.slug)) lessons
