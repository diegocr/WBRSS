/* ***** BEGIN LICENSE BLOCK *****
 * Version: MIT/X11 License
 * 
 * Copyright (c) 2006 Diego Casorran
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 * 
 * Contributor(s):
 *   Diego Casorran <dcasorran@gmail.com> (Original Author)
 * 
 * ***** END LICENSE BLOCK ***** */


#ifndef DEBUG_H
#define DEBUG_H

#include <clib/debug_protos.h>

#ifdef DEBUG
static __inline void __dEbuGpRInTF( CONST_STRPTR func_name, CONST LONG line )
{
	KPutStr( func_name );
	KPrintF(",%ld: ", line );
}
# define DBG( fmt... ) \
({ \
	__dEbuGpRInTF(__PRETTY_FUNCTION__, __LINE__); \
	KPrintF( fmt ); \
})
# define DBG_POINTER( ptr )	DBG("%s = 0x%08lx\n", #ptr, (long) ptr )
# define DBG_VALUE( val )	DBG("%s = 0x%08lx,%ld\n", #val,val,val )
# define DBG_STRING( str )	DBG("%s = 0x%08lx,\"%s\"\n", #str,str,str)
# define DBG_ASSERT( expr )	if(!( expr )) DBG(" **** FAILED ASSERTION '%s' ****\n", #expr )
# define DBG_EXPR( expr, bool )						\
({									\
	BOOL res = (expr) ? TRUE : FALSE;				\
									\
	if(res != bool)							\
		DBG("Failed %s expression for '%s'\n", #bool, #expr );	\
									\
	res;								\
})
#else
# define DBG(fmt...)		((void)0)
# define DBG_POINTER( ptr )	((void)0)
# define DBG_VALUE( ptr )	((void)0)
# define DBG_STRING( str )	((void)0)
# define DBG_ASSERT( expr )	((void)0)
# define DBG_EXPR( expr, bool )	expr
#endif

#define DBG_TRUE( expr )	DBG_EXPR( expr, TRUE )
#define DBG_FALSE( expr )	DBG_EXPR( expr, FALSE)

#define ENTER()	DBG("--- ENTERING %s:%ld\n", __func__, __LINE__)
#define LEAVE()	DBG("--- LEAVING %s:%ld\n", __func__, __LINE__)

#endif /* DEBUG_H */
