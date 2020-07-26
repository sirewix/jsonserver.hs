{-# LANGUAGE OverloadedStrings #-}

module Search where
import           App
import           Misc
import           Data.Text                      ( Text )
import           Data.Text.Encoding
import           Database.PostgreSQL.Simple     ( query
                                                , execute
                                                , Only(..)
                                                )
import           Entities
import           Logger
import qualified Data.Aeson                    as J


posts () (log, db) =
  catchDb log (return BadRequest) $ do undefined



{-
API новостей должно поддерживать фильтрацию по полям:
день создания (созданные ранее даты, созданные после даты, созданные в тот же день),
Примеры:
/posts?created_at=2018-05-21
/posts?created_at__lt=2018-05-21
/posts?created_at__gt=2018-05-21

имя автора,
категория по айди,
тег по айди
Примеры:
/posts?tag=123
/posts?tags__in=[123,124,125]
/posts?tags__all=[123,124,125]

поиск по конкретному тегу
найти статьи, в которых есть хоть один тег из списка
найти только те статьи, в которых есть все теги одновременно,
название (вхождение подстроки)
контент (вхождение подстроки)
API новостей должно поддерживать поиск по строке, которая может быть найдена либо в текстовом контенте, либо в имени автора, либо в названии категории/тега
API новостей должно поддерживать сортировку по:
дате,
автору (имя по алфавиту),
по категориям (название по алфавиту),
по количеству фотографий
-}
