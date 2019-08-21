#ifndef WARP_LANG_READER_H
#define WARP_LANG_READER_H

#include <stdio.h>
#include <wchar.h>
#include <stdint.h>
#include <stdbool.h>
#include "../errors.h"
#include "utils.h"
#include "source.h"
#include "lexer.h"
#include "pool.h"
#include "ast.h"


RetVal tryExprRead(Pool_t pool, TokenStream_t stream, Expr **expr, Error *error);


#endif //WARP_LANG_READER_H
