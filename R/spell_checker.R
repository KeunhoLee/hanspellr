#' Spell Checker for R
#'
#' test version
#'
#' @param texts text for spell check, Korean.
#' @return length 1 character, spell corrected text.
#' @examples
#'   spell_checker('마춤뻡 검사')
#' @export
spell_checker <- function(texts){

  if(nchar(texts) > 500) return(FALSE)

  base_url <- 'https://m.search.naver.com/p/csearch/ocontent/spellchecker.nhn'

  result <- httr::GET(base_url,
                      query = list(`_callback`='window._jindo2._spellingCheck_0',
                                   q = texts),
                      httr::add_headers(`user-agent`='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36',
                                  referer = 'https://search.naver.com/'),
                      encode = 'UTF-8')

  if(result$status_code != 200) return(FALSE)


  result <- httr::content(result, 'text')

  result <- gsub('_callback', '', result)
  result <- gsub('\\(', '[', result)
  result <- gsub('\\)', ']', result)
  result <- gsub(';', '', result)
  result <- gsub('\n', '', result)
  result <- gsub('window._jindo2._spellingCheck_0', '', result)

  result <- jsonlite::fromJSON(result)

  return(result$message$result$notag_html)

}
