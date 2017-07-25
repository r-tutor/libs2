#include <Rcpp.h>

#ifdef __linux__
#include <tidy/tidy.h>
#include <tidy/buffio.h>
#endif

#ifdef __APPLE__
#include <tidy.h>
#include <tidybuffio.h>
#endif

// libtidy docs:
// http://api.html-tidy.org/tidy/tidylib_api_5.2.0/tidyenum_8h.html#a3a1401652599150188a168dade7dc150

// NOTE: cannot do "using namespace Rcpp;" b/c of annoying warnings about the
//       ambiguity of 'yes'.

//' Tidy HTML/XML
//'
//' @param source length 1 character vetor containing the HTML/XML source to process
//' @export
//[[Rcpp::export]]
std::string tidy(std::string source) {

  TidyBuffer output = {0};
  TidyBuffer errbuf = {0};
  int rc = -1;
  Bool ok;

  TidyDoc tdoc = tidyCreate();

  ok = tidyOptSetBool(tdoc, TidyXhtmlOut, yes);

  if (ok == no) Rcpp::stop("Error setting TidyHTML options");

  rc = tidySetErrorBuffer(tdoc, &errbuf);

  if (rc<0) Rcpp::stop("Error setting TidyHTML error buffer");

  rc = tidyParseString(tdoc, source.c_str());

  if (rc<0) Rcpp::stop("Error parsing source document");

  rc = tidyCleanAndRepair(tdoc);

  if (rc<0) Rcpp::stop("Error tidying source document");

  rc = tidyRunDiagnostics(tdoc);

  if (rc<0) Rcpp::stop("Error generating tidy diagnostics");

  rc = tidySaveBuffer(tdoc, &output);

  if (rc<0) Rcpp::stop("Error converting parsed document to character vector");

  std::string ret = std::string(reinterpret_cast<const char*>(output.bp));

  tidyBufFree(&output);
  tidyBufFree(&errbuf);
  tidyRelease(tdoc);

  return(ret);

}
