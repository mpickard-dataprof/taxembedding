##############################################################
##############################################################

library(stringr)

## custom list of tax-specific stopwords
tax_stopwords <- c(
  "section",
  "title",
  "subsec",
  "subsection",
  "pub",
  "provided",
  str_c(1:1200),
  "i",
  "ii",
  "iii",
  "iiii",
  "iv",
  "xix",
  "xi",
  "note",
  "act",
  "amendment",
  "amendments",
  "date",
  "par",
  "paragraph",
  "ch",
  "chapter",
  "stat",
  "subpar",
  "added",
  "rule",
  "code",
  "i.r.c",
  "effective",
  "jan",
  "preceding",
  "sentence",
  "regulations",
  "regulation",
  "prescribed",
  "beginning",
  "relating",
  "related",
  "provisions",
  "amending",
  "sections",
  "similar",
  "amended",
  "amend",
  "inserted",
  "reference",
  "references",
  "subparagraph"
)

####

####
# $1.25 --> $1_25
# $70,000 --> $70_000
# $17,964.25 --> $17_964_25
# $2 million --> $2_million
# $15.5 billion --> $15_5_billion
preserve_currency_references <- function(line) {
  ##line <- "\aI02Over $70,000 but not over $125,000 \aD\aoi2$17,964.25, plus 36% of the excess over $70,000. Don't match 2.45. do match $1.3. don't match 12,560, do match $1.45 $2 million, $60 million, $15.5 million, $2.5 million $7.3 billion, $100 billion, $.7 billion $1.56 billion don't match 4.56 don't match 1.4 do $1.23 "

  ## just need to match $xx.x "million" and "billion" and insert underscore before alpha descriptor
  ## then match currency with $'s

  ## -- match $xx.xx (million|trillion) and put underscore before (million|trillion) --
  # cases from USC26 include: $2 million, $60 million, $15.56 million, $2.5 million
  # $7.3 billion, $100 billion, $.7 billion
  alpha_num_currency <- "(?<=\\$(\\d{0,3})(\\.)?(\\d{1,3})?)( )(?=(million|billion))"

  # match commas that come AFTER a $ and 1-3 digits (have to also allow for zero or more
  # sets of comma and three digits) and BEFORE 3 digits
  comma_in_currency <- "(?<=\\$(\\d{1,3}(\\,\\d{3}){0,4}))(\\,)(?=\\d{3})"

  # match decimals AFTER a $ (and possibly some numbers with commas) and BEFORE 1-2 digits
  # NOTE: commas will already be replaced with underscores, so checking for underscores to
  # separate thousands, millions, etc.
  decimal_in_currency <- "(?<=\\$(\\d{1,3}(\\_\\d{3}){0,4})?)(\\.)(?=\\d{1,2})"

  invisible (
    line %>%
      str_replace_all(alpha_num_currency, "_") %>%
      str_replace_all(comma_in_currency, "_") %>%
      str_replace_all(decimal_in_currency, "_")
  )

}

####
# 36% --> 36_percent
# 34.5% --> 34_5_percent
# 15.5-percent --> 15_5_percent
preserve_percent_references <- function(line) {
  ## line <- "34.5% 15.5-percent 45.75 percent 2.75-percent 15-percent \aI02Over $70,000 but not over $125,000 \aD\aoi2$17,964.25, plus 36% of the excess over $70,000. 34.5 percent. 55 percentage"

  # Examples from USC26 include:
  # 39.6 percent
  # 15-percent
  # 36 percent
  # 3 percent
  # 200 percent

  ## NOTE: I found one case of "thirty-five percent", but figure such cases will be either 1)
  ## immaterial or 2) will handled by n-gram windows.
  decimal_percent <- "(?<=(\\d{0,3}))(\\.)(?=(\\d{1,2}%|\\d{0,2}[ -]percent))"
  alpha_num_percent <- "(?<=(\\d{0,3})(\\_\\d{1,2})?)([ -])(?=percent)"

  # TODO: Fix alpha_num_percent pattern to not match "7th such taxable year, 1/3 of the percentage"
  invisible (
    line %>%
      str_replace_all(decimal_percent, "_") %>%
      str_replace_all(alpha_num_percent, "_") %>%
      str_replace_all("%", "_percent")
  )

}


# remove other wierd characters
# IMPORTANT: this needs to go
remove_other_characters <- function(line) {

  invisible (
    line %>%
      stringr::str_remove_all("\\\\\\d*\\\\") %>% # remove footnote references
      stringr::str_remove("[^[:print:]]") # other non-alphanumeric characters
  )
}


# After a reference is identified, str_replace_all calls this function
# to replace certain spaces and parentheses with underscores
# this is the functioning portion of the replace_references function
standardize_reference <- function(line)
{
  # if the heirarchy level ends with s, remove the s
  # Ex: paragraphs -> paragraph, titles -> title
  line <- line %>% str_remove(pattern = "(?<=\\w)s(?=\\b)")

  # extract the first word (which will be the hierarchical level)
  level <- tolower(str_extract(string = line, pattern = "\\w*(?=\\s)"))
  repl <- paste(" ", level, " ", sep = "")

  # find all instances of a comma followed by a space within the match
  ref1 <- "(?<=,)\\s(?!and|or)|(?<=and|or)\\s"

  # find all spaces within a match
  ref2 <- "(?<!,|or|and)\\s(?!,|or|and)"

  # find all open parentheses within a match
  ref3 <- "(?<!_)\\("

  # find all parentheses that are followed (or superceded) by an underscore
  ref4 <- "\\)(?=_)|(?<=_)\\("

  # find the last, closing parentheses within a match
  ref5 <- "\\)"

  # find all section symbols
  ref6 <- "§"

  # find all dashes and periods
  ref7 <- "\\.|-"

  invisible(
    line %>%
      # replace compound references with single references
      str_replace(pattern = "\\w*(?=\\s)", replacement = level) %>%
      str_replace_all(pattern = ref1, replacement = repl) %>%
      # replace single references
      str_replace_all(pattern = ref2, replacement = "_") %>%
      str_replace_all(pattern = ref3, replacement = "_") %>%
      str_remove_all(pattern = ref4) %>%
      str_remove_all(pattern = ref5) %>%
      # replace eCFR references
      str_remove_all(pattern = ref6) %>%
      str_replace_all(pattern = ref7, replacement = "_")
  )
}



# preserve external and internal references
# I give some brief examples of what each pattern matches here, but
# I have a more comprehensive list of the test cases I used for each regex expression
# define a function to replace practice references
replace_references <- function(line)
{
  # define all external references
  # EXAMPLES: Title 9 * Title 18a * titles 9 and 10 * Titles 9, 12a, 13, or 15
  title_ref <- "\\b[Tt]itles?(?>\\s?(?>(?>\\d{1,4}\\w{0,2})|(?>(?>(?>and)|(?>or)))),?)*"

  # EXAMPLES: Subtitle A * subtitles A or B * Subtitles A, B, C * subtitles A, C, D, E, or F
  subtitle_ref <- "\\b[Ss]ubtitles?(?>\\s?(?>(?>\\b\\w\\b)|(?>(?>(?>and)|(?>or)))),?)*"

  # EXAMPLES: Chapter 2A * chapter 100 * Chapters 6, 7, 8 * chapters 6A, 7, 72, or 10
  chapter_ref <- "\\b[Cc]hapters?(?>\\s?(?>(?>\\d{1,4}\\w?)|(?>(?>(?>and)|(?>or)))),?)*"

  # EXAMPLES: subchapter A * subchapters A, B, C * Subchapters A, B, C, or F * Subchapter A and B
  subchapter_ref <- "\\b[Ss]ubchapters?(?>\\s?(?>(?>\\b\\w\\b)|(?>(?>(?>and)|(?>or)))),?)*"

  # EXAMPLES: part II * parts I, II, or IV * parts II and I * part II and III
  part_ref <- "\\b[Pp]arts?(?>\\s?(?>(?>\\b[:upper:]{1,4}\\b)|(?>(?>(?>and)|(?>or)))),?)*"

  # EXAMPLES: Subpart A * subparts A, D, E * Subparts A, D, and Z * subpart A or B
  subpart_ref <- "\\b[Ss]ubparts?(?>\\s?(?>(?>\\b[:upper:]\\b)|(?>(?>(?>and)|(?>or)))),?)*"

  # EXAMPLES: section 2503 * section 2012(d) * section 2010(c)(3) * Sections 2432(d)(3), 2351(c), or 1123
  section_ref <- "\\b[Ss]ections?(?>\\s?(?>(?>\\d{1,4}?(?>\\(\\w{1,3}\\))*)|(?>(?>(?>and)|(?>or)))),?)*"




  # define all internal references
  # EXAMPLES: subsection (d) * Subsection (e)(1)(C) * subsections (b)(1), (c), (e)(1)(C) * subsection (b)(1) or (b)(2)
  subsection_ref <- "\\b[Ss]ubsections?(?>\\s?(?>(?>\\(\\w{1,3}\\),?)|(?>(?>(?>and)|(?>or)))))*"

  # EXAMPLES: paragraph (2) * paragraphs (1)(B) and (2)(C) * paragraphs (1)(C), (2)(G)(iii), and (5) * paragraph (1)(A) and (3)(F)
  paragraph_ref <- "\\b[Pp]aragraphs?(?>\\s?(?>(?>\\(\\w{1,3}\\))|(?>(?>(?>and)|(?>or)))),?)*"

  # EXAMPLES: subparagraph (A) * Subparagraph (A)(i)(I) * Subparagraph (B)(iii) or (D)(i)(I) * subparagraph (A) or subparagraph (B)
  subparagraph_ref <- "\\b[Ss]ubparagraphs?(?>\\s?(?>(?>\\(\\w{1,4}\\))|(?>(?>(?>and)|(?>or)))),?)*"

  # EXAMPLES: Clause (i) * clause (iii)(III)(aa) * clauses (ii)(III)(ab), (i)(IV), (i) * clauses (i), (ii)(ab)(III), (iv)(I) and (iii)
  clause_ref <- "\\b[Cc]lauses?(?>\\s?(?>(?>\\(\\w{1,4}\\))|(?>(?>(?>and)|(?>or)))),?)*"

  # EXAMPLES: Subclause (III)(ab)(AB) * subclause (I) or (II) * Subclauses (I), (V), (IX), and (X) * subclauses (I) and (IV)
  subclause_ref <- "\\b[Ss]ubclauses?(?>\\s?(?>(?>\\(\\w{1,4}\\))|(?>(?>(?>and)|(?>or)))),?)*"

  # EXAMPLES: item (bb)(AB)(aaa) * item (ac)(AB)(aaf) and (df)(BB) * Items (ai)(AC), (ak)(BE)(ffe), (ak), or (am) * items (aa) and (ab)
  item_ref <- "\\b[Ii]tems?(?>\\s?(?>(?>\\(\\w{1,4}\\))|(?>(?>(?>and)|(?>or)))),?)*"

  # EXAMPLES: subitem (AB)(aaa) * subitem (AC) and (AD)(aac) * Subitems (DE), (AL)(aae), and (AB) * subitems (AA) and (AB)
  subitem_ref <- "\\b[Ss]ubitems?(?>\\s?(?>(?>\\(\\w{1,4}\\))|(?>(?>(?>and)|(?>or)))),?)*"

  # EXAMPLES: Subsubitem (aaa) * Subsubitems (aac), (aaa), (aac) * Subsubitems (eei), (ffe), (bff), or (aae) * Subsubitems (aad) or (aal)
  subsubitem_ref <- "\\b[Ss]ubsubitems?(?>\\s?(?>(?>\\(\\w{1,4}\\))|(?>(?>(?>and)|(?>or)))),?)*"




  # define all eCFR references
  # EXAMPLES: §1.401(a)(4)-3(d)(2)(ii)(B) * §1.401(k)-6 * §1.421-6(d)(2) * §§521.101 to 521.117
  eCFR_ref <- "(?>§*(?>\\d|\\w)+\\.(?>\\d|\\w)+(?>\\(\\w{1,4}\\))*)-?(?>(?>\\d|\\w)+(?>\\(\\w{1,4}\\))*)*"



  # first, replace all of the references
  invisible(
    line %>%
      # replace the external references
      str_replace_all(pattern = title_ref, replacement = standardize_reference) %>%
      str_replace_all(pattern = subtitle_ref, replacement = standardize_reference) %>%
      str_replace_all(pattern = chapter_ref, replacement = standardize_reference) %>%
      str_replace_all(pattern = subchapter_ref, replacement = standardize_reference) %>%
      str_replace_all(pattern = part_ref, replacement = standardize_reference) %>%
      str_replace_all(pattern = subpart_ref, replacement = standardize_reference) %>%
      str_replace_all(pattern = section_ref, replacement = standardize_reference) %>%
      # replace the internal references
      str_replace_all(pattern = subsection_ref, replacement = standardize_reference) %>%
      str_replace_all(pattern = paragraph_ref, replacement = standardize_reference) %>%
      str_replace_all(pattern = subparagraph_ref, replacement = standardize_reference) %>%
      str_replace_all(pattern = clause_ref, replacement = standardize_reference) %>%
      str_replace_all(pattern = subclause_ref, replacement = standardize_reference) %>%
      str_replace_all(pattern = item_ref, replacement = standardize_reference) %>%
      str_replace_all(pattern = subitem_ref, replacement = standardize_reference) %>%
      str_replace_all(pattern = subsubitem_ref, replacement = standardize_reference) %>%
      # replace the eCFR references
      str_replace_all(pattern = eCFR_ref, replacement = standardize_reference)
  )
}


# helper function for str_replace_all call in preserve_ngrams_in_corpus.
# It replaces spaces with underscores inside the match
join_ngram <- function(line) {
  return (
    line %>% str_replace_all("[:blank:]", "_")
  )
}

# helper function to paste each ngram size together
create_ngram_regex <- function(df, ngram_size) {
  return (
    df %>% filter(size == ngram_size) %>% pull(ngram) %>% str_c(collapse = "|")
  )
}


# helper function to split corpus into equal parts
chunk_vector <- function(x,n) {
  split(x, cut(seq_along(x), n, labels = FALSE))
}



preserve_ngrams_in_corpus <- function(corpus) {

  library(readr)
  library(stringr)
  library(furrr)
  library(parallel)

  plan(multiprocess)

  # load list ngrams to preserve
  if (!exists("tax_ngrams")) {
    tax_ngrams <- read_csv(
      "data/output_data/ngram_counts.csv",
      col_types = cols(
        ngram = col_character(),
        size = col_integer(),
        n = col_integer()
      )
    )
  }

  # assemble the regex pattern--largest ngrams first
  # the ngrams were generated empirically with other code, basically
  # selected ngrams that occur most frequently.
  if (!exists("ngram_patterns")) {
    sizes <- rev(unique(tax_ngrams$size))
    ngram_patterns <-
      map_chr(sizes, create_ngram_regex, df = tax_ngrams)
    ngram_patterns <- str_c(ngram_patterns, collapse = "|")
  }

  # chunk the corpus based on number of available cores
  # numCores <- detectCores()
  numCores <- 8L
  corpus_chunks <- chunk_vector(corpus, numCores)

  replace_ngram_in_chunk <- function(chunk, pattern) {
    chunk_list <- map(chunk, str_replace_all, pattern, replacement = join_ngram)
    invisible(
      unlist(unname(chunk_list))
    )
  }

  corpus <- future_map(corpus_chunks, replace_ngram_in_chunk, pattern = ngram_patterns)

  invisible (unname(unlist(corpus)))
}


## PURPOSE: Find and replace things that that will otherwise be ripped apart
## by the tokenizer.
## INPUT: a vector of untokenized documents (i.e., an array of strings)
## OUTPUT: a vector of cleaned, untokenized documents (i.e., an array strings)
prepare_corpus <- function(corpus,
                        preserve_currency = TRUE,
                        preserve_percent = TRUE,
                        preserve_ngrams = TRUE,
                        preserve_references = TRUE) {

  if (preserve_currency) {
    corpus <- preserve_currency_references(corpus)
  }

  if (preserve_percent) {
    corpus <- preserve_percent_references(corpus)
  }

  if (preserve_ngrams) {
    corpus <- preserve_ngrams_in_corpus(corpus)
  }

  if (preserve_references) {
    corpus <- replace_references(corpus)
  }

  ## IMPORTANT: Call 'remove_other_characters()' last
  corpus <- remove_other_characters(corpus)

  invisible(corpus)

}

## PURPOSE: Tokenize the corpus into words or word stems.
## INPUT: a vector of cleaned, untokenized documents (i.e., an array of strings)
## OUTPUT: a vector of tokenized documents (i.e., an array of (stem) word arrays )
tokenize_corpus <- function(corpus,
                            remove_stopwords = TRUE,
                            remove_tax_stopwords = TRUE,
                            stem_words = FALSE) {
  library(tokenizers)
  library(stopwords)

  # assemble stopwords lists
  sw <- vector()

  if (remove_stopwords) {
    sw <- c(sw, stopwords())
  }

  if (remove_tax_stopwords) {
    sw <- c(sw, tax_stopwords)
  }

  if (stem_words) {
    tokens <- tokenize_word_stems(corpus,
                                  stopwords = sw)
  } else {
    tokens <- tokenize_words(corpus,
                             stopwords = sw,
                             strip_punct = TRUE,
                             strip_numeric = TRUE)
  }

  invisible(tokens)

}
