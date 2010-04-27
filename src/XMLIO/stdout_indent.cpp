#include "xmlio_std.hpp"
#include "stdout_indent.hpp"
#include "stdout.hpp"

int iostreamIndexIndent=ios::xalloc();


static struct CInitStdOutIndent
{
  CInitStdOutIndent()
  {
    cout<<ResetIndent ;
    cerr<<ResetIndent ;
    clog<<ResetIndent ;
  } ;
} Init ;
