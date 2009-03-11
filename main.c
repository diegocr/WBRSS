/* ***** BEGIN LICENSE BLOCK *****
 * Version: MIT/X11 License
 * 
 * Copyright (c) 2009 Diego Casorran
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


#define _PROGRAM_NAME		"WBRSS"
#define _PROGRAM_VERZ		"1.0"
#define _PROGRAM_DATE		"01.02.2007"
#define _PROGRAM_COPY		"Copyright (C) 2007 Diego Casorran - http://amiga.sf.net/"

#if __GNUC__ > 2
# error the program is buggy while compiled with gcc 3.0+ (check text_xmax and fix it...)
#endif

//------------------------------------------------------------------------------

char __bsdsocketname[]="bsdsocket.library",__openurlname[]="openurl.library";

#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/intuition.h>
#include <proto/utility.h>
#include <proto/alib.h>
#include <proto/icon.h>
#include <proto/diskfont.h>
#include <proto/ttengine.h>
#include <proto/openurl.h>
#include <graphics/text.h>
#include <graphics/gfxbase.h>
#include <proto/diskfont.h>
#include <proto/graphics.h>
#include <graphics/gfxmacros.h>
#include <proto/datatypes.h>
#include <datatypes/pictureclass.h>
#include <SDI_compiler.h>
#include <workbench/startup.h>
#include <dos/dostags.h>
#include <proto/socket.h>
#include <netdb.h>
#include <stdarg.h>
#include "debug.h"

#ifdef USE_XML2SHARED
# include <proto/xml2.h>
# include <xml2/parser.h>
# include <xml2/tree.h>
STATIC struct Xml2Base * Xml2Base = NULL;
#else
# include "xmlsupport.h"
#endif

STATIC CONST UBYTE __VerID[] = 
 "$VER:" _PROGRAM_NAME " " _PROGRAM_VERZ " " _PROGRAM_DATE " " _PROGRAM_COPY ;

#define NW_BORDER	4
#define FEEDSP		6
#define FAVICON_WIDTH	16 /* CONST value for favicon.ico images (width and height) */
#define FAVICON_HEIGHT	FAVICON_WIDTH
// normal RSS logo is 29x14 (width x height)
#define RSSLOGO_WIDTH	30
#define RSSLOGO_HEIGHT	14
#define NW_BOXWIDTH	(RSSLOGO_WIDTH + (NW_BORDER*4))
#define NW_HEIGHT	(FAVICON_WIDTH + (NW_BORDER*2))
#define WMINHEIGHT	(NW_NEIGHT+2)

#define DEFAULT_FONT_SIZE	15
#define FONT	((struct TextFont *) data->font )

#define DBG_AXIS( name )						\
	DBG("%s :: xmin = %ld, ymin = %ld, xmax = %ld, ymax = %ld\n",	\
	#name,name ## _xmin, name ## _ymin, name ## _xmax, name ## _ymax)

/*****************************************************************************/

typedef enum
{
	FE_NONE, // if none or unknown...
	FE_UTF8,
	FE_UTF16BE,
	FE_UTF16LE,
	FE_UTF32BE,
	FE_UTF32LE
} FeedEncoding;

typedef struct item
{
	struct item * next;
	
	unsigned char * text; // that text is "title: description"
	unsigned char * link;
} * Item;

typedef struct feed
{
	struct feed * next;
	
	STRPTR url;
	STRPTR hostname;
	char url_error;
	UBYTE favicon[64]; // points to a local file (temp on T:)
	char favicon_error;
	Item item;
	FeedEncoding enc;
} * Feed;

typedef struct data
{
	ULONG magic;
	#define DATAMAGIC	0x90014752
	
	// multi-threaded data
	struct Task * task, * child;
	struct MsgPort * mPort;
	ULONG mPortSig;
	
	// program data
	char * progfilename;
	char * font_name;
	
	UWORD left, top, width, font_size, boxcolor;
	long text_pen, bgcolor, delay, update_interval, task_pri;
	struct Window * window;
	struct TextFont * wFont; // orig window->RPort->Font
	APTR font;
	BOOL font_isTrueType;
	UBYTE layout;
	STRPTR pubscreen;
	STRPTR sepimg; // image used to separate news items
	BOOL use_favicon;
	
	Feed feeds;
	
} * Data;

typedef enum
{
	EIF_START	= 0x01,	// new item starting
	EIF_END		= 0x02,	// item end
	
	EIF_TEXT_BOLD	= 0x03,	// change font style to FST_BOLD
	EIF_TEXT_NORMAL	= 0x04,
	
} eif_t; /* embeed (rss) item flag _ type */

typedef unsigned char * StringChar;
typedef unsigned long   StringLong;

typedef struct
{
	StringChar str;		// data
	StringLong len;		// length of data
	StringLong size;	// max size data can have
	
} String;

typedef enum
{
	IPCA_NOACTION,
	IPCA_SHOWMESSAGE,
	IPCA_DRAWTEXT
} IPCACT_T;

typedef enum { IPCR_OK, IPCR_FAIL, IPCR_ABORTED } IPCRES_T;

struct IPCMsg
{
	struct Message  ipc_msg;
	unsigned long   ipc_ID;
	IPCACT_T        ipc_action;
	APTR            ipc_data;
	union
	{
		LONG            ipc_res_result;
		struct MsgPort *ipc_res_port;
	} ipc_res;
};

#define ipc_result	ipc_res.ipc_res_result
#define ipc_port	ipc_res.ipc_res_port
#define IPC_MAGIC	0x9ffff444

STATIC Data data = NULL;
STATIC struct SignalSemaphore * sem = NULL;
STATIC struct Library *TTEngineBase = NULL;
STATIC UWORD TextYAxis = 0;

GLOBAL unsigned char * HTMLToText( unsigned char * htmlstring );

#define NEED_LIBRARY( libname, libver )	\
	tell("%s v%ld required!", libname, libver )

#define malloc(size)	AllocVec((size)+2, MEMF_PUBLIC)
#define free(ptr)	FreeVec( ptr );

#define MAKECOLOR32(x) (((x)<<24)|((x)<<16)|((x)<<8)|(x))

#ifndef _between
# define _between(a,x,b)	\
	((x)>=(a) && (x)<=(b))
#endif

#define _isinarea(o,x,y) \
	(_between(o ##_xmin,(x),o ##_xmax) && _between(o ##_ymin,(y),o ##_ymax))

/*****************************************************************************/
/* utils */

struct RawDoFmtStream {
	
	STRPTR Buffer;
	LONG Size;
};

static void RawDoFmtChar( REG(d0,UBYTE c), REG(a3,struct RawDoFmtStream *s))
{
	if(s->Size > 0)
	{
		*(s->Buffer)++ = c;
		 (s->Size)--;
		
		if(s->Size == 1)
		{
			*(s->Buffer)	= '\0';
			  s->Size	= 0;
		}
	}
}

#undef snprintf
#define snprintf	__my_snprintf
#undef vsnprintf /* undef needed for gcc 3.0+ ... */
#define vsnprintf	__my_vsnprintf

STATIC LONG vsnprintf(STRPTR outbuf, LONG size, CONST_STRPTR fmt, _BSD_VA_LIST_ args)
{
	long rc = 0;
	
	if(/*(size > (long)sizeof(char)) &&*/ (outbuf != NULL))
	{
		struct RawDoFmtStream s;
		
		s.Buffer = outbuf;
		s.Size	 = size;
		
		RawDoFmt( fmt, (APTR)args, (void (*)())RawDoFmtChar, (APTR)&s);
		
		if((rc = ( size - s.Size )) != size)
			--rc;
	}
	
	return(rc);
}

STATIC LONG snprintf( STRPTR outbuf, LONG size, CONST_STRPTR fmt, ... )
{
	va_list args;
	long rc;
	
	va_start (args, fmt);
	rc = vsnprintf( outbuf, size, fmt, args );
	va_end(args);
	
	return(rc);
}

/*****************************************************************************/
#define memset		__memset
#define bzero		__bzero

INLINE void memset(REG(a0, void *data), REG(a1, char bit), REG(d0, long size))
{
	register char * ptr = (char *) data;
	
	while(size-- > 0)
		*ptr++ = bit;
}

INLINE VOID bzero( REG(a0, void *data), REG(d0, long size ))
{
	register unsigned long * uptr = (unsigned long *) data;
	register unsigned char * sptr;
	
	// first blocks of 32 bits
	while(size >= (long)sizeof(ULONG))
	{
		*uptr++ = 0;
		size -= sizeof(ULONG);
	}
	
	sptr = (unsigned char *) uptr;
	
	// now any pending bytes
	while(size-- > 0)
		*sptr++ = 0;
}

void bcopy(const void *src,void *dest,size_t len)
{
	CopyMem((APTR) src, dest, len );
}

/*****************************************************************************/
// The following two functions comes from the clib2 sources, (c) Olaf Barthel

STATIC struct SignalSemaphore * __create_semaphore( void )
{
	struct SignalSemaphore * semaphore;

	#if defined(__amigaos4__)
	{
		semaphore = AllocSysObject(ASOT_SEMAPHORE,NULL);
	}
	#else
	{
		semaphore = AllocVec(sizeof(*semaphore),MEMF_ANY|MEMF_PUBLIC);
		if(semaphore != NULL)
			InitSemaphore(semaphore);
	}
	#endif /* __amigaos4 */

	return(semaphore);
}

STATIC void __delete_semaphore(struct SignalSemaphore * semaphore)
{
	if(semaphore != NULL)
	{
		#if defined(__amigaos4__)
		{
			FreeSysObject(ASOT_SEMAPHORE,semaphore);
		}
		#else
		{
			DBG_ASSERT( semaphore->ss_Owner == NULL );

			#if defined(DEBUG)
			{
				/* Just in case somebody tries to reuse this data
				   structure; this should produce an alert if
				   attempted. */
				memset(semaphore,0,sizeof(*semaphore));
			}
			#endif /* DEBUG */

			FreeVec(semaphore);
		}
		#endif /* __amigaos4 */
	}
}

/*****************************************************************************/

INLINE ULONG Time( VOID )
{
	struct DateStamp ds;
	ULONG __result;
	
	DateStamp(&ds);
	
	__result= ds.ds_Days * 86400
		+ ds.ds_Minute * 60
		+ (ds.ds_Tick / TICKS_PER_SECOND);
	
	return __result;
}

/*****************************************************************************/

STATIC BOOL putMsg( struct MsgPort *destino, IPCACT_T action, APTR udata )
{
	struct MsgPort *replyport = NULL;
	BOOL error = TRUE;
	
	if(!destino)
		return FALSE;
	
	ENTER();
	
	if((replyport = CreateMsgPort()))
	{
		struct IPCMsg ipcmsg;
		APTR xMsg;
		
		ipcmsg.ipc_msg.mn_ReplyPort	= replyport;
		ipcmsg.ipc_msg.mn_Length	= sizeof(struct IPCMsg);
		ipcmsg.ipc_ID			= IPC_MAGIC;
		ipcmsg.ipc_action		= action;
		ipcmsg.ipc_result		= IPCR_ABORTED;
		ipcmsg.ipc_data			= udata;
		
		DBG("Sending action '%ld' from %lx to %lx\n", action, replyport, destino);
		
		Forbid();
		PutMsg( destino, &ipcmsg.ipc_msg);
		WaitPort(replyport);
		while((xMsg = GetMsg( replyport )))
		{
			DBG("Got reply...\n");
			
			switch(((struct IPCMsg *)xMsg)->ipc_result)
			{ // TODO
				case IPCR_ABORTED:
					DBG("IPCR_ABORTED\n");
					break;
				
				case IPCR_FAIL:
					DBG("IPCR_FAIL\n");
					break;
				
				case IPCR_OK:
					DBG("IPCR_OK\n");
					break;
				default:
					break;
			}
		}
		Permit();
		
		DeleteMsgPort(replyport);
		
		error = FALSE;
	}
	
	LEAVE();
	
	return !error;
}

/*****************************************************************************/

/*****************************************************************************/

STATIC ULONG strlen(const char *string)
{ const char *s=string;
  
  if(!(string && *string))
  	return 0;
  
  do;while(*s++); return ~(string-s);
}
STATIC char * strstr( const char *a, const char *b)
{
	int l = strlen(b);
	for( ; *a ; a++ ) if(Strnicmp(a, b, l) == 0) return (char *)a;
	return NULL;
}

STATIC STRPTR StrDup(STRPTR str)
{
	STRPTR new = NULL;

	if(str != NULL)
	{
		ULONG len = strlen(str);
		
		if((new = AllocVec(len+1, MEMF_PUBLIC)))
		{
			CopyMem( str, new, len );
			new[len] = 0;
		}
	}
	
	return new;
}

STATIC STRPTR GetValue( STRPTR string, STRPTR outbuf, LONG outlen )
{
	UBYTE ech;
	
	while(*string && *string++ != '=');
	
	ech = ((*string == '\"') ? '\"' : 0x20);
	if(ech != 0x20)
		string++;
	
	while(*string && *string != ech && --outlen > 0)
	{
		*outbuf++ = *string++;
	}
	*outbuf = 0;
	
	return((outlen > 0) ? string : NULL);
}

STATIC LONG ShowRequestA( STRPTR gadgets, CONST_STRPTR fmt, va_list args )
{
	struct EasyStruct ErrReq = {
		sizeof (struct EasyStruct), 0, NULL, NULL, NULL
	};
	
	ErrReq.es_Title        = (STRPTR) __VerID + 5;
	ErrReq.es_TextFormat   = (STRPTR) fmt;
	ErrReq.es_GadgetFormat = gadgets;
	
	DBG_STRING(fmt);
	
	return EasyRequestArgs(NULL, &ErrReq, NULL, args );
}
#if 0
STATIC LONG ShowRequest( STRPTR gadgets, CONST_STRPTR fmt, ... )
{
	va_list args;
	LONG result;
	
	va_start (args, fmt);
	result = ShowRequestA( gadgets, fmt, args );
	va_end( args );
	
	return result;
}
#endif
STATIC VOID tell( CONST_STRPTR fmt, ...)
{
	ShowRequestA( "Ok", fmt, &fmt+1 );
}

/*****************************************************************************/

#define TEMP_PATH	"T:"

STATIC STRPTR GetTempFile( STRPTR out, ULONG outlen, char * ext )
{
	BPTR fd = 0;
	do {
		snprintf( out, outlen, "%swbrss.%08lx.%s", TEMP_PATH, Time(), ext != NULL ? ext : "tmp");
		if(fd) UnLock(fd);
	} while((fd = Lock( out, SHARED_LOCK )));
	
	return out;
}

INLINE VOID RemoveTempFiles ( VOID )
{
	struct FileInfoBlock *fib;
	
	if((fib=(struct FileInfoBlock *)AllocDosObject(DOS_FIB,0)))
	{
		BPTR lock;
		
		if((lock=Lock( TEMP_PATH, SHARED_LOCK)))
		{
			BPTR oldDir = CurrentDir( lock );
			
			if(Examine(lock,fib)!=DOSFALSE)
			{
				while(ExNext(lock,fib)!=DOSFALSE)
				{
					if(strstr( fib->fib_FileName, _PROGRAM_NAME ))
					{
						DeleteFile ( fib->fib_FileName );
					}
				}
			}
			CurrentDir(oldDir);
			UnLock(lock);
		}
		FreeDosObject(DOS_FIB,fib);
	}
}

/*****************************************************************************/

#define MEMFLAGS MEMF_PUBLIC

STATIC String * string_new( long size )
{
	String * str;
	
	if(size <= 0)
		size = 1024;
	
	if((str = AllocMem(sizeof(*str), MEMFLAGS)))
	{
		if((str->str = AllocMem(str->size = size, MEMFLAGS)))
		{
			str->str[str->len = 0] = 0;
		}
		else {
			FreeMem( str, sizeof(*str));
		}
	}
	
	return str;
}

STATIC void string_free( String * s )
{
	if(s != NULL)
	{
		if(s->str != NULL)
			FreeMem( s->str, s->size );
		
		FreeMem( s, sizeof(*s));
	}
}

STATIC BOOL string_resize(String * s, StringLong lentocopy)
{
	if(s->len + lentocopy >= s->size)
	{
		StringChar new_str;
		StringLong new_size;
		
		new_size = s->size + lentocopy + 256;
		
		if(!(new_str = AllocMem( new_size, MEMFLAGS )))
			return FALSE;
		
		//memcpy( new_str, s->str, s->len );
		CopyMem( s->str, new_str, s->len );
		new_str[s->len] = 0;
		
		FreeMem( s->str, s->size );
		
		s->str  = new_str;
		s->len  = s->len;
		s->size = new_size;
	}
	
	return TRUE;
}

STATIC int string_append(String *s, StringChar str, StringLong len)
{
	if(((long)len) <= 0)
		len = strlen(str);
	
	if(((long)len) <= 0)
		return -1;
	
	if(!string_resize( s, len ))
		return -1;
	
	//memcpy (s->str + s->len, str, len);
	CopyMem( str, s->str + s->len, len );
	s->len += len;
	
	s->str[s->len] = 0;
	
	return len;
}

/*****************************************************************************/

STATIC int FindPos( unsigned char * pajar, unsigned char * aguja )
{
	const unsigned char *src = (const unsigned char *) pajar;
	
	if(!(pajar && *pajar) || !(aguja && *aguja))
		return 0;
	
	while(*src)
	{
		const unsigned char * a = (const unsigned char *) aguja;
		
		while(*src && (*src++ == *a++))
		{
			if(!(*a))
				return ~(pajar-++src);
		}
	}
	
	return 0;
}

/*****************************************************************************/

STATIC VOID FreeData( void )
{
	ObtainSemaphore(sem);
	
	ENTER();
	
	if(data != NULL)
	{
		Feed feed = data->feeds, fn;
		
		while( feed )
		{
			Item item, in;
			
			fn = feed->next;
			
			for( item = feed->item ; item ; item = in )
			{
				in = item->next;
				
				if(item->text != NULL)
					FreeVec( item->text );
				if(item->link != NULL)
					FreeVec( item->link );
				FreeMem( item, sizeof(*item));
			}
			
			if(feed->hostname != NULL)
				FreeVec( feed->hostname );
			if(feed->url != NULL)
				FreeVec( feed->url );
			FreeMem( feed, sizeof(*feed));
			feed = fn;
		}
		
		if( data->progfilename )
			FreeVec( data->progfilename );
		if( data->font_name )
			FreeVec( data->font_name );
		if( data->pubscreen )
			FreeVec( data->pubscreen );
		
		FreeMem( data, sizeof(*data));
		data = NULL;
	}
	
	LEAVE();
	
	ReleaseSemaphore(sem);
}


#define TTVALUE( tooltype, ttlen, uw ) \
	if(!Strnicmp( TT, tooltype, ttlen )) \
	{ \
		if(StrToLong( buf, &val ) == -1) \
		{ \
			tell("%s on tooltype value %s failed!", "StrToLong", tooltype); \
			goto error; \
		} \
		data->uw = (UWORD) val; \
		DBG("%s set to %04lx,%ld\n", tooltype, data->uw, data->uw ); \
	}

#define TTSTRING( tooltype, ttlen, uw ) \
	if(!Strnicmp( TT, tooltype, ttlen )) \
	{ \
		if(!(data-> uw = StrDup( buf ))) \
			goto error; \
		DBG("%s set to %08lx,%s\n", tooltype, data->uw, data->uw ); \
	}

#define TTBOOL( tooltype, ttlen, uw ) \
	if(!Strnicmp( TT, tooltype, ttlen )) \
	{ \
		switch(*buf) \
		{ \
			case 'Y': \
			case '1': \
				data-> uw = 1; \
				break; \
			case 'N': \
			case '0': \
				data-> uw = 0; \
				break; \
			default: \
				data-> uw = *buf - '0'; \
				break; \
		} \
		DBG("%s set to %04lx,%ld\n", tooltype, data->uw, data->uw ); \
	}

STATIC BOOL SetData( struct WBStartup * _WBenchMsg )
{
	BOOL rc = FALSE;
	LONG saved_ioerr = 0;
	struct DiskObject *dob;
	UBYTE __progname[256], buf[4096];
	
	__progname[0] = '\0';
	if(!(data = (Data) AllocMem(sizeof(*data), MEMF_PUBLIC|MEMF_CLEAR)))
		return FALSE;
	
	data->magic = DATAMAGIC;
	data->text_pen	= (UWORD)-1;
	data->bgcolor	= (UWORD)-1;
	data->boxcolor	= (UWORD)-1;
	
	if(_WBenchMsg)
	{
		if (NameFromLock(GetProgramDir(), buf, sizeof(buf)-1))
		{
			if (AddPart(buf, _WBenchMsg->sm_ArgList[0].wa_Name, sizeof(buf)-1))
			{
				data->progfilename = StrDup(buf);
			}
		}
	}
	else
	{
		if (NameFromLock(GetProgramDir(), buf, sizeof(buf)-1))
		{
			if (GetProgramName( __progname, sizeof(__progname)-1))
			{
				if (AddPart(buf, __progname, sizeof(buf)-1))
				{
					data->progfilename = StrDup(buf);
				}
			}
		}
	}
	
	if( data->progfilename == NULL )
		goto error;
	
	if(!(dob = GetDiskObject(data->progfilename)))
		goto error;
	{
		STRPTR *arry = dob->do_ToolTypes;
		BOOL error = FALSE;
		
		while(*arry)
		{
			long val = 0;
			STRPTR TT = *arry++;
			
			if(!Strnicmp( TT, "***", 3)) break;
			
			// get tooltype value
			GetValue( TT, buf, sizeof(buf)-1);
			
			if(!buf[0]) continue;
			
			     TTVALUE( "LEFT"     , 4 , left )
			else TTVALUE( "TOP"      , 3 , top )
			else TTVALUE( "WIDTH"    , 5 , width )
			else TTVALUE( "FONTSIZE" , 8 , font_size )
			else TTVALUE( "TEXTPEN"  , 7 , text_pen )
			else TTVALUE( "BGCOLOR"  , 7 , bgcolor )
			else TTVALUE( "BOXCOLOR" , 8 , boxcolor )
			else TTVALUE( "DELAY"    , 5 , delay )
			else TTVALUE( "UPDATE"   , 6 , update_interval )
			else TTVALUE( "TASKPRI"  , 7 , task_pri )
			else TTSTRING("FONTNAME" , 8 , font_name )
			else TTSTRING("PUBSCREEN", 9 , pubscreen )
			else TTSTRING("SEPIMG"   , 6 , sepimg )
			else TTBOOL( "LAYOUT"    , 6 , layout )
			else TTBOOL( "FAVICON"   , 7 , use_favicon )
			else if(!Strnicmp( TT, "URL", 3 ))
			{
				Feed feed;
				STRPTR a,b;
				
				if(!(feed = AllocMem(sizeof(*feed), MEMF_PUBLIC|MEMF_CLEAR)))
					goto error;
				
				if(!(feed->url = StrDup( buf )))
				{
					saved_ioerr = IoErr();
					FreeMem( feed, sizeof(*feed));
					goto error;
				}
				
				if(!(feed->hostname = AllocVec(strlen(buf)+4, MEMF_PUBLIC)))
				{
					saved_ioerr = IoErr();
					FreeVec( feed->url );
					FreeMem( feed, sizeof(*feed));
					goto error;
				}
				
				a = buf;
				b = feed->hostname;
				if( a  && !Strnicmp( a, "http://", 7))
					a += 7;
				
				while( *a && *a != '/' )
					*b++ = *a++;
				*b = 0;
				
				feed->next = data->feeds;
				data->feeds = feed;
			}
		}
		
		FreeDiskObject(dob);
		
		if((rc = !error && (data->feeds != NULL)))
		{
			// if less than 10 minutes adjust it
			if(data->update_interval < 10)
				data->update_interval = 10;
			
			// I'll count it in seconds
			data->update_interval *= 60;
			
			if(!Stricmp( data->font_name + strlen(data->font_name) - 4, ".ttf" ))
			{
				DBG("Using TrueType Font\n");
				data->font_isTrueType = TRUE;
			}
		}
		
		if( data->feeds == NULL )
			SetIoErr( ERROR_REQUIRED_ARG_MISSING );
	}
	
	return(rc);
error:
	if( saved_ioerr == 0 )
		saved_ioerr = IoErr();
	
	FreeData(  );
	SetIoErr( saved_ioerr );
	return FALSE;
}

/*****************************************************************************/

INLINE LONG Connect( STRPTR hostname, UWORD port, struct Library *SocketBase )
{
	long sockfd = -1, i;
	struct hostent *hostaddr;
	struct sockaddr_in saddr;
	
	if(!(hostaddr = gethostbyname( hostname )))
	{
		return -1;
	}
   	
	if((sockfd = socket(AF_INET, SOCK_STREAM, 0)) == -1)
	{
		return -1;
	}
	
	memset( &saddr, 0, sizeof(struct sockaddr_in));
	
	saddr.sin_len		= sizeof(struct sockaddr_in);
	saddr.sin_family	= AF_INET;
	saddr.sin_port		= htons( port );
	
	for( i = 0 ; hostaddr->h_addr_list[i] ; i++ )
	{
		CopyMem( hostaddr->h_addr_list[i], &saddr.sin_addr, (ULONG)hostaddr->h_length);
		
		if(! connect( sockfd, (struct sockaddr *)&saddr, sizeof(saddr)))
			break;
	}
	
	if( ! hostaddr->h_addr_list[i] )
	{
		// connection failed
		CloseSocket( sockfd );
		sockfd = -1;
	}
	
	return sockfd;
}

INLINE LONG Receive(long sock, void *buf, long len, long flags, struct Library * SocketBase)
{
	fd_set rdfs;
	LONG result = 0;
	struct timeval timeout = { 4, 0 };
	
	FD_ZERO(&rdfs);
	FD_SET(sock, &rdfs);
	
	if(WaitSelect( sock+1, &rdfs, NULL, NULL, &timeout, NULL))
		result = recv( sock, buf, len , flags );
	
	return( result );
}

INLINE String *GetURL( STRPTR url, LONG * error, struct Library *SocketBase)
{
	STRPTR host, path, furl = url;
	int maxlen, ulen = strlen(url);
	
	STRPTR urlhost = NULL;
	STRPTR urlpath = NULL;
	STRPTR request = NULL;
	
	String * rdata = NULL;
	
	long sockfd = -1;
	long port = 80;
	
	int MAX_HOSTLEN	= 256;
	int MAX_PATHLEN	= ulen;
	
	ENTER();
	
	(*error) = 0;
	
	if(!Strnicmp( url, "http://", 7))
		url += 7;
	
	if(!(host = urlhost = malloc(MAX_HOSTLEN)) || !(path = urlpath = malloc(MAX_PATHLEN)))
	{
		(*error) = ERROR_NO_FREE_STORE;
		goto done;
	}
	
	maxlen = MAX_HOSTLEN;
	do {
		*host++ = *url++;
		
	} while( *url && *url != '/' && *url != ':' && --maxlen > 0);
	
	if(maxlen < 1)
	{
		(*error) = ERROR_BUFFER_OVERFLOW;
		goto done;
	}
	
	if(*url == ':')
	{
		long chs;
		
		chs = StrToLong( ++url, &port );
		
		// if a invalid port was found, fall back to standard number
		if(!(port > 0 && port < 65536))
			port = 80;
		
		url += chs;
	}
	
	maxlen = MAX_PATHLEN;
	if(!*url)
		*path++ = '/';
	else do {
		*path++ = *url++;
		
	} while( *url && --maxlen > 0);
	
	// check for overflow
	if(maxlen < 1)
	{
		(*error) = ERROR_BUFFER_OVERFLOW;
		goto done;
	}
	
	// null terminate hostname and server path
	*path = 0;
	*host = 0;
	
	if((sockfd = Connect(urlhost,port&0xffff,SocketBase)) == -1 )
	{
		(*error) = ERROR_LOCK_TIMEOUT;
		goto done;
	}
	
	maxlen = ((strlen(furl)*2)+strlen(urlhost)+1024);
	
	//DBG("Allocated buffer for request is %ld bytes\n", maxlen );
	
	if(!(request = malloc( maxlen+2 )))
	{
		(*error) = ERROR_NO_FREE_STORE;
		goto done;
	}
	
	maxlen = snprintf( request, maxlen,
		"GET %s HTTP/1.0\r\n"
		"User-Agent: %s\r\n"
		"Host: %s\r\n"
		"\r\n", urlpath, __VerID + 5, urlhost );
	
	if(send( sockfd, request, maxlen,0) != maxlen)
	{
		(*error) = ERROR_WRITE_PROTECTED;
		goto done;
	}
	
	if(!(rdata = string_new( 512 )))
	{
		(*error) = ERROR_NO_FREE_STORE;
		goto done;
	}
	
	//while((maxlen = recv( sockfd, sockbuf, sizeof(sockbuf)-1, 0))>0)
	//while((maxlen = Receive( sockfd, sockbuf, sizeof(sockbuf)-1, 0, SocketBase))>0)
	while((maxlen = Receive( sockfd, &rdata->str[rdata->len], rdata->size-rdata->len-1, 0, SocketBase))>0)
	{
		//if(string_append( rdata, sockbuf, maxlen ) < 0)
		rdata->str[rdata->len += maxlen] = 0;
		if(!string_resize( rdata, 1024 ))
		{
			shutdown( sockfd, 2);
			(*error) = ERROR_NO_FREE_STORE;
			goto done;
		}
	}
	
done:
	// break singal received?
	if(Errno() == 4 || (SetSignal(0L,SIGBREAKF_CTRL_C) & SIGBREAKF_CTRL_C))
	{
		(*error) = ERROR_BREAK;
	}
	
	if( sockfd != -1 )
	{
		CloseSocket(sockfd);
		sockfd = -1;
	}
	
	if( rdata != NULL )
	{
		if(  rdata->str != NULL )
		{
			long val;
			
			StrToLong( &rdata->str[9], &val );
			
			if( val != 200 )
			{
				(*error) = ERROR_OBJECT_NOT_FOUND;
			}
		}
	}
	
	if((*error) != 0)
	{
		if( rdata != NULL )
		{
			string_free( rdata );
			rdata = NULL;
		}
	}
	
	if( urlhost )
		free( urlhost );
	if( urlpath )
		free( urlpath );
	if( request )
		free( request );
	
	LEAVE();
	
	return rdata;
}

/*****************************************************************************/

#define UTF_UNTRASLATED	0xA7

static const struct { UWORD utf; UBYTE latin; } __faked_utf8_table[] = 
{
	// The General punctuation range 8192-8303
	{ 8208, '-' }, { 8209, '-' }, { 8210, '-' }, { 8211, '-' }, { 8212, '-' },
	{ 8213, '-' }, { 8214, '|' }, { 8215, '_' }, { 8216, '\''}, { 8217, '\''},
	{ 8218, ',' }, { 8220, '\"'}, { 8221, '\"'}, { 8222, ',' }, { 8224, '+' },
	{ 8225, '+' }, { 8226, '·' }, { 8230,  0  }, { 8240, '%' }, { 8242, '\''},
	{ 8243, '\"'}, { 8249, '<' }, { 8250, '>' }, { 8260, '/' },
	// The Currency Symbols range 8352-8399
	{ 8364, '¤' },
	// The Letterlike Symbols range 8448-8527
	{ 8465, 'J' }, { 8472, 'P' }, { 8476, 'R' }, { 8482,  0  }, { 8486,  0  },
	{ 8501, 'N' },
	// no more fakes :=)
	{ 0,0 }
};

INLINE LONG __translate_utf8_to_latin1(REG(a0, STRPTR str), REG(d0, ULONG strLen))
{
	register CONST_STRPTR src = (CONST_STRPTR) str;
	register STRPTR dst = str;
	register STRPTR srcend;
	unsigned int c, d;
	int trailing;
	
	if( str == NULL )
		return 0;
	
	if( strLen < 1 )
		strLen = strlen( str );
	
	if( strLen < 1 )
		return 0;
	
	srcend = str + strLen;
	while( src < srcend )
	{
		d = *src++;
		
		if( d < 0x80 )  { c= d; trailing= 0; }
		else if (d < 0xC0)
		{
			/* trailing byte in leading position */
			
			// normal behaviour is to fail, I'll continue...
			//return -1;
			c= d; trailing= 0;
		}
		else if (d < 0xE0)  { c= d & 0x1F; trailing= 1; }
		else if (d < 0xF0)  { c= d & 0x0F; trailing= 2; }
		else if (d < 0xF8)  { c= d & 0x07; trailing= 3; }
		else {
			/* no chance for this in IsoLat1 */
			
			// normal behaviour is to fail, I'll continue...
			//return -1;
			c= d; trailing= 0;
		}
		
		if(srcend - src < trailing)
			break;
		
		while( trailing-- )
		{
			if(src >= srcend)
				break;
			if(((d= *src++) & 0xC0) != 0x80)
			{
				// normal behaviour is to fail, I'll continue...
				//return -1;
				c = UTF_UNTRASLATED;
				break;
			}
			
			c <<= 6;
			c |= d & 0x3F;
		}
		
		/* assertion: c is a single UTF-4 value */
		if( c <= 0xFF )
		{
			*dst++ = c;
		}
		else
		{	/* no chance for this in IsoLat1 */
			
			// normal behaviour is to fail, I'll Fake it...
			
			int i = -1;
			
			while(__faked_utf8_table[++i].utf != 0)
			{
				if(__faked_utf8_table[i].utf == c)
				{
					if(__faked_utf8_table[i].latin)
						*dst++ = __faked_utf8_table[i].latin;
					break;
				}
			}
		}
	}
	
	*dst = 0;
	
	c = dst - str;
	
	return c;
}

/*****************************************************************************/

STATIC LONG save_item( Feed feed, xmlNodePtr item )
{
	xmlNodePtr iptr;
	xmlChar * title = NULL, * description = NULL, * link = NULL;
	LONG error = 0;
	
//	ENTER();
	
	for( iptr = item ; item ; item = item->next )
	{
		if(!xmlStrcasecmp( item->name, "title"))
		{
			title = xmlNodeListGetString( item->doc, item->xmlChildrenNode, 1);
		}
		else if(!xmlStrcasecmp( item->name, "description"))
		{
			description = xmlNodeListGetString( item->doc, item->xmlChildrenNode, 1);
		}
		else if(!xmlStrcasecmp( item->name, "link"))
		{
			link = xmlNodeListGetString( item->doc, item->xmlChildrenNode, 1);
		}
		
		if((title != NULL) && (description != NULL) && (link != NULL))
			break;
	}
	
	if((title != NULL) || (description != NULL))
	{
		Item ni;
		
		if((ni = AllocMem(sizeof(*ni),MEMF_PUBLIC)))
		{
			ULONG size = strlen(title)+strlen(description)+FEEDSP+40;
			
			ni->link = NULL;
			if((ni->text = AllocVec( size+1, MEMF_PUBLIC)))
			{
				unsigned char * ptr = ni->text;
				int len;
				
				*ptr++ = EIF_START;
				
				if(title != NULL)
				{
					// show the title in bold
					*ptr++ = EIF_TEXT_BOLD;
					
					len = snprintf( ptr, size, "%s%s", title, ((description != NULL) ? ": ":""));
					ptr += len;
					
					// disable bold
					*ptr++ = EIF_TEXT_NORMAL;
				}
				
				if(description != NULL)
				{
					len = snprintf( ptr, size, description );
					ptr += len;
				}
				
				for( len = FEEDSP ; len-- >= 0 ; *ptr++ = ' ');
				*ptr++ = EIF_END;
				*ptr = '\0';
				
				HTMLToText( ni->text );
				HTMLToText( ni->text ); // yes, two times MAY is needed
				
				if( data->font_isTrueType == FALSE )
				{
					if( feed->enc == FE_UTF8 )
						__translate_utf8_to_latin1( ni->text, 0 );
				}
				
				if( link != NULL )
				{
					if((ni->link = StrDup( link )) != NULL)
						HTMLToText( ni->link ); // mainly &amp; -> &
				}
				
				ObtainSemaphore(sem);
				ni->next = feed->item;
				feed->item = ni;
				ReleaseSemaphore(sem);
			}
			else
			{
				error = ERROR_NO_FREE_STORE;
				FreeMem(ni, sizeof(*ni));
			}
		}
		else error = ERROR_NO_FREE_STORE;
	}
	
	if(title != NULL)
		xmlFree(title);
	if(description != NULL)
		xmlFree(description);
	if(link != NULL)
		xmlFree(link);
	
//	LEAVE();
	
	return error;
}

INLINE LONG xml2lib_parser(Feed feed, xmlNodePtr root)
{
	xmlNodePtr node;
	LONG error = 0;
	
	ENTER();
	
	for( node = root ; node ; node = node->next )
	{
		if(!xmlStrcasecmp( node->name, "channel")) // process RSS format
		{
			xmlNodePtr child = node->xmlChildrenNode;
			
			for( ; child ; child = child->next )
			{
				if(!xmlStrcasecmp( child->name, "item"))
				{
					if((error = save_item( feed, child->xmlChildrenNode )))
						break;
				}
			}
		}
		else if(!xmlStrcasecmp( node->name, "item"))
		{
			if((error = save_item( feed, node->xmlChildrenNode )))
				break;
		}
	}
	
	LEAVE();
	
	return error;
}

INLINE FeedEncoding GetEncodingType( STRPTR xml_data, ULONG xml_len )
{
	FeedEncoding rc = FE_NONE;
	
	while(xml_len-- > 0)
	{
		if(!Strnicmp( xml_data, "encoding", 8 ))
		{
			UBYTE encstr[32];
			
			if(GetValue( xml_data, encstr, sizeof(encstr)) != NULL)
			{
				if(!Strnicmp( encstr, "UTF-8", 5))
					rc = FE_UTF8;
				else if(!Strnicmp( encstr, "UTF-16LE", 8))
					rc = FE_UTF16LE;
				else if(!Strnicmp( encstr, "UTF-16", 6))
					rc = FE_UTF16BE;
				else if(!Strnicmp( encstr, "UTF-32LE", 8))
					rc = FE_UTF32LE;
				else if(!Strnicmp( encstr, "UTF-32", 6))
					rc = FE_UTF32BE;
			}
			
			break;
		}
		
		if(*++xml_data == '>') break;
	}
	
	DBG_VALUE(rc);
	return( rc );
}

INLINE LONG ParseXML( Feed feed, STRPTR xml_data, ULONG xml_len )
{
	LONG error = ERROR_INVALID_RESIDENT_LIBRARY;
	
	ENTER();
	
	#ifdef DEBUG
	{BPTR fd;
	UBYTE file[33];
	if((fd = Open(GetTempFile( file, sizeof(file),NULL), MODE_NEWFILE )))
	{
		Write( fd, xml_data, xml_len );
		Close( fd );
	}}
	#endif /* DEBUG */
	
	#ifdef USE_XML2SHARED
	if((Xml2Base = (struct Xml2Base *) OpenLibrary("xml2.library", 4)))
	#endif
	{
		xmlDoc *doc = NULL;
		xmlNode *root = NULL;
		
		if((doc = xmlParseMemory( xml_data, xml_len )) != NULL)
		{
			if((root = xmlDocGetRootElement(doc)) != NULL)
			{
				if(xmlStrcasecmp( root->name, "RDF") && xmlStrcasecmp( root->name, "RSS"))
				{
					error = ERROR_OBJECT_WRONG_TYPE;
				}
				else
				{
					feed->enc = GetEncodingType( xml_data, xml_len );
					
					error = xml2lib_parser(feed,root->xmlChildrenNode);
				}
			}
			else error = ERROR_FILE_NOT_OBJECT;
			
			xmlFreeDoc(doc);
			xmlCleanupParser();
		}
		else error = ERROR_FILE_NOT_OBJECT;
		
		#ifdef USE_XML2SHARED
		CloseLibrary((struct Library *)Xml2Base);
		#endif
	}
	
	LEAVE();
	
	return error;
}

/*****************************************************************************/

INLINE VOID GetFavicon ( Feed feed, struct Library *SocketBase )
{
	String * rdata;
	UBYTE favicon[256];
	BOOL ok = FALSE;
	long error;
	
	if(!( feed->hostname && *feed->hostname && data->use_favicon && !feed->favicon_error ))
		return;
	
	ObtainSemaphore(sem);
	snprintf( favicon, sizeof(favicon)-1, "http://%s/favicon.ico", feed->hostname);
	
	if((rdata = GetURL( favicon, &error, SocketBase )))
	{
		long start;
		
		if((start = FindPos( rdata->str, "\r\n\r\n")))
		{
			BPTR fd;
			
			if((fd = Open(GetTempFile( feed->favicon, sizeof(feed->favicon), "ico"), MODE_NEWFILE)))
			{
				Write( fd, &rdata->str[start], rdata->len - start );
				Close( fd );
				ok = TRUE;
			}
		}
		string_free( rdata );
	}
	
	if( ok == FALSE )
		feed->favicon_error = 1;
	
	ReleaseSemaphore(sem);
}

/*****************************************************************************/

STATIC LONG SubtaskTimesWorked = 0, SubtaskTimesWorkedOK = 0;

static void subtask( void )
{
	struct Library *SocketBase = NULL;
//	struct Task *ThisChild = FindTask(NULL);
	Feed feed;
	BOOL GotSomething = FALSE;
	
	Wait(SIGF_SINGLE);
	Forbid();
	Signal( data->task, SIGF_SINGLE );
	Permit();
	
	ENTER();
	
	while(!(SocketBase = OpenLibrary(__bsdsocketname, 4)))
	{
		putMsg( data->mPort, IPCA_DRAWTEXT, "Waiting for TCP/IP to be running...");
		Delay( TICKS_PER_SECOND * 2 );
		if(SetSignal(0L,SIGBREAKF_CTRL_C) & SIGBREAKF_CTRL_C)
			goto exit;
	}
	
	putMsg( data->mPort, IPCA_DRAWTEXT, "updating feeds...");
	
	for( feed = data->feeds ; feed ; feed = feed->next )
	{
		String * page;
		long error = 0;
		
		// a previous error notifyed already?
		if( feed->url_error )
			continue;
		
		if(!(page = GetURL( feed->url, &error, SocketBase )))
		{
			UBYTE buffer[1024], ioerrstr[82];
			
			DBG("Error %ld fetching \"%s\"\n", error, feed->url);
			
			Fault( error, NULL, ioerrstr, sizeof(ioerrstr) - 1 );
			
			snprintf( buffer, sizeof(buffer)-1,
				"%s RSS from:\n\"%s\"\n\nError %ld: %s",
					"Cannot fetch", feed->url, error, ioerrstr );
			
			putMsg( data->mPort, IPCA_SHOWMESSAGE, buffer );
			
			switch( error )
			{
				case ERROR_NO_FREE_STORE:
				case ERROR_BREAK:
					goto exit;
				
				case ERROR_BUFFER_OVERFLOW:
					feed->url_error = TRUE;
					break;
				
				default: // non-fatal errors
					break;
			}
		}
		else
		{
			long start;
			
			if((start = FindPos( page->str, "\r\n\r\n")))
			{
				if( data->use_favicon )
					GetFavicon ( feed, SocketBase ) ;
				
				if((error=ParseXML( feed, &page->str[start], page->len - start )))
				{
					UBYTE buffer[1024], ioerrstr[82];
					
					Fault( error, NULL, ioerrstr, sizeof(ioerrstr) - 1 );
					
					snprintf( buffer, sizeof(buffer)-1,
						"%s RSS from:\n\"%s\"\n\nError %ld: %s",
							"Error processing", feed->url, error, ioerrstr );
					
					putMsg( data->mPort, IPCA_SHOWMESSAGE, buffer );
					feed->url_error = TRUE;
				}
				else
				{
					GotSomething = TRUE;
				}
			}
			string_free( page );
		}
	}
	
	if( GotSomething == FALSE )
	{
		if( SubtaskTimesWorkedOK < 1 )
			putMsg( data->mPort, IPCA_DRAWTEXT, "error fetching rss news");
	}
	else SubtaskTimesWorkedOK++;
	SubtaskTimesWorked++;
	
exit:
	if( SocketBase )
		CloseLibrary( SocketBase );
	
	LEAVE();
	
	Forbid ( );
	Signal( data->task, SIGBREAKF_CTRL_E );
}

STATIC BOOL CreateSubTask( VOID )
{
	BOOL rc = FALSE;
	
	ENTER();
	
	data->child = (struct Task *) CreateNewProcTags (
			NP_Entry,	(ULONG) &subtask,
			NP_Name,	(ULONG) FindTask(NULL)->tc_Node.ln_Name,
			NP_StackSize,	65536,
		TAG_DONE );
	
	DBG_POINTER(data->child);
	
	if(data->child != NULL)
	{
		Signal( data->child, SIGF_SINGLE );
		Wait(SIGF_SINGLE);
		rc = TRUE;
	}
	
	LEAVE();
	
	return rc;
}

/*****************************************************************************/

#ifdef USE_XML2SHARED
STATIC BOOL IsXml2Available ( VOID )
{
	BOOL rc = FALSE;
	
	if(!(Xml2Base = (struct Xml2Base *) OpenLibrary("xml2.library", 4)))
	{
		NEED_LIBRARY("xml2.library", 4);
	}
	else
	{
		CloseLibrary((struct Library *)Xml2Base);
		rc = TRUE;
	}
	
	return TRUE;
}
#else
# define IsXml2Available() (TRUE)
#endif

/*****************************************************************************/

STATIC struct TextFont * SystemFont ( UBYTE Size )
{
	struct TextAttr ta;
	struct TextFont * font;
	
	Forbid();
	ta.ta_Name  = GfxBase->DefaultFont->tf_Message.mn_Node.ln_Name;
	ta.ta_YSize = Size ? Size : GfxBase->DefaultFont->tf_YSize;
	ta.ta_Style = GfxBase->DefaultFont->tf_Style;
	ta.ta_Flags = GfxBase->DefaultFont->tf_Flags;
	
	font = (struct TextFont *) OpenDiskFont(&ta);
	Permit();
	
	return font;
}

STATIC struct TextFont * DefaultFont ( VOID )
{
	struct TextFont * font;
	struct TextAttr ta;
	
	if(!(font = SystemFont ( 0 )))
	{
		ta.ta_Name = "topaz.font";
		ta.ta_YSize = 8;
		ta.ta_Style = 0;
		ta.ta_Flags = 0;
		
		font = (struct TextFont *) OpenFont(&ta);
	}
	
	return font;
}

INLINE VOID OpenMyFont ( VOID )
{
	if(!data->font_size)
		data->font_size = DEFAULT_FONT_SIZE;
	
	// fall back to DefaultFont() if no ttengine.library is available
	if( data->font_isTrueType )
	{
		if(!(TTEngineBase = OpenLibrary("ttengine.library", 7 )))
		{
			NEED_LIBRARY("ttengine.library",7);
		}
		else
		{
			data->font = TT_OpenFont(
				TT_FontFile, (ULONG) data->font_name,
				TT_FontSize, data->font_size,
			TAG_DONE );
			
			if( ! data->font )
			{
				tell("cannot open %s:%ld", FilePart(data->font_name), data->font_size);
				CloseLibrary( TTEngineBase );
				TTEngineBase = NULL;
			}
		}
		
		if( ! data->font ) // no way to use a TrueType font
		{
			FreeVec( data->font_name );
			data->font_name = NULL;
			data->font_isTrueType = FALSE;
		}
	}
	
	if( ! data->font && data->font_name != NULL )
	{
		struct TextAttr ta;
		
		ta.ta_Name  = data->font_name;
		ta.ta_YSize = data->font_size;
		ta.ta_Style = 0;
		ta.ta_Flags = 0;
		
		if(!(data->font = OpenDiskFont(&ta)))
			tell("cannot open %s:%ld", data->font_name, data->font_size);
	}
	
	if( ! data->font )
	{
		data->font = DefaultFont ( ) ;
	}
}

INLINE BOOL CreateWindow ( VOID )
{
	struct Screen * scr = NULL;
	BOOL rc = FALSE;
	
	if((scr = LockPubScreen(data->pubscreen)))
	{
		UWORD height = 20;
		
		// open the font we'll use later
		OpenMyFont ( ) ;
		
		if(!data->left) data->left = 50;
		if(!data->top) data->top = 90;
		if(!data->width) data->width = 400;
		if(data->font)
		{
			if( data->font_isTrueType )
			{
				// this is guaranteed to be exact? or how
				// to get tf_YSize BEFORE opening the window?
				
				if( data->font_size < 20 )
					height = data->font_size + 8;
				else if( data->font_size < 30 )
					height = data->font_size + 10;
				else
					height = data->font_size + 14;
			}
			else
			{
				height = FONT->tf_YSize + 2;
			}
		}
		
		if( data->layout > 0 )
		{
			height += (NW_BORDER * 2) + 4;
		}
		
		data->window = OpenWindowTags(NULL,
			WA_Left,		data->left,
			WA_Top,			data->top,
			WA_Width,		data->width,
			WA_Height,		height,
			WA_DragBar,		FALSE,
			WA_CloseGadget,		FALSE,
			WA_SizeGadget,		FALSE,
			WA_DepthGadget,		FALSE,
			WA_Borderless,		TRUE,
			WA_Activate,		FALSE,
			WA_Backdrop,		TRUE,
			WA_SmartRefresh,	TRUE,
			WA_NoCareRefresh,	TRUE,
			WA_IDCMP,		IDCMP_CLOSEWINDOW|IDCMP_MOUSEBUTTONS|IDCMP_RAWKEY,
			WA_PubScreen,		(ULONG) scr,
			WA_PubScreenFallBack,	TRUE,
		TAG_DONE);
		
		UnlockPubScreen(NULL, scr );
		
		if((rc = (data->window != NULL)) == FALSE)
		{
			tell("cannot open window!");
		}
		
		if( data->font != NULL )
		{
			if( rc == FALSE )
			{
				if( data->font_isTrueType )
					TT_CloseFont( data->font );
				else
					CloseFont( data->font );
				data->font = NULL;
			}
			else
			{
				data->wFont = data->window->RPort->Font;
				
				if( data->font_isTrueType )
				{
					TT_SetFont( data->window->RPort, data->font );
				}
				else
				{
					SetFont( data->window->RPort, data->font );
					SetSoftStyle( data->window->RPort, FONT->tf_Style, 7);
				}
			}
		}
	}
	else tell("cannot lock pubscreen!");
	
	return rc;
}

INLINE VOID DeleteWindow( VOID )
{
	if( data->font != NULL )
	{
		if( data->font_isTrueType )
			TT_DoneRastPort( data->window->RPort );
		
		// if data->font != NULL, data->wFont MUST be also
		SetFont( data->window->RPort, data->wFont);
		SetSoftStyle( data->window->RPort, data->wFont->tf_Style, 7);
		
		if( data->font_isTrueType )
			TT_CloseFont( data->font );
		else
			CloseFont( data->font );
	}
	
	CloseWindow( data->window );
}

/*****************************************************************************/

STATIC void RectFillOutline(struct RastPort *rp, WORD xmin, WORD ymin, WORD xmax, WORD ymax, LONG p1, LONG p2)
{
	ULONG orig_apen;
	
	// backup A-pen
	orig_apen = GetAPen( rp );
	
	// draw box
	SetAPen(rp,p1);
	Move(rp, xmin, ymax);
	Draw(rp, xmin, ymin);
	Draw(rp, xmax, ymin);
	SetAPen(rp,p2);
	Draw(rp, xmax, ymax);
	Draw(rp, xmin, ymax);
	
	// restore a-pen
	SetAPen( rp, orig_apen );
}

STATIC VOID TextShadow(struct RastPort * rp, WORD x, WORD y, STRPTR text, UWORD textLen, UBYTE PenColor, UBYTE ShadowColor, UBYTE BPen )
{
	ULONG orig_apen, orig_bpen;
	
	// backup pens
	orig_apen = GetAPen( rp );
	orig_bpen = GetBPen( rp );
	
	// write shadowed text
	SetBPen( rp, BPen );
	SetAPen( rp, ShadowColor );
	Move( rp, x+2, y+2 );
	Text( rp, text, textLen );
	SetAPen( rp, PenColor );
	Move( rp, x, y );
	Text( rp, text, textLen );
	
	// restore pens
	SetAPen( rp, orig_apen );
	SetBPen( rp, orig_bpen );
}

STATIC VOID TextDraw( STRPTR text )
{
	UWORD len = (UWORD) strlen( text );
	struct RastPort * rp = data->window->RPort;
	
	ULONG apen = GetAPen( rp );
	ULONG drmod = GetDrMd( rp );
	
	SetAPen( rp, 2 );
	SetDrMd( rp, JAM2);
	Move( rp, NW_BOXWIDTH+20, TextYAxis );
	if( data->font_isTrueType )
		TT_Text( rp, text, len );
	else
		Text( rp, text, len );
	SetAPen( rp, apen );
	SetDrMd( rp, drmod);
}

/*****************************************************************************/

STATIC VOID mPortDispatch( VOID )
{
	struct IPCMsg * ipcmsg;
	
	// clear the signal
	SetSignal(0L,data->mPortSig);
	
	DBG("Got signal at my port\n");
	
	// get the next waiting message
	while((ipcmsg = (struct IPCMsg *)GetMsg( data->mPort )))
	{
		DBG_VALUE(ipcmsg->ipc_ID);
		
		if(ipcmsg->ipc_ID == IPC_MAGIC)
		{
			switch( ipcmsg->ipc_action )
			{
				case IPCA_SHOWMESSAGE:
					tell((CONST_STRPTR) ipcmsg->ipc_data );
					ipcmsg->ipc_result = IPCR_OK;
					break;
				
				case IPCA_DRAWTEXT:
					TextDraw((STRPTR) ipcmsg->ipc_data );
					ipcmsg->ipc_result = IPCR_OK;
					break;
				
				default:
					break;
			}
		}
		
		ReplyMsg((APTR) ipcmsg );
	}
}

/*****************************************************************************/

INLINE VOID About ( VOID )
{
	tell( _PROGRAM_NAME " " _PROGRAM_VERZ " " _PROGRAM_DATE "\n" _PROGRAM_COPY );
}

/*****************************************************************************/

INLINE VOID __main_loop ( struct WBStartup * _WBenchMsg UNUSED )
{
	struct Window * window	= data->window;
	struct RastPort * rp	= window->RPort;
	int DISPLAY_HEIGHT	= window->Height;
	int DISPLAY_WIDTH	= window->Width;
	struct Message *msg = NULL;
	UWORD bl, textheight, scrollX, scrollY, y, ly, tlen;
	UWORD favicon_xmin=0, favicon_ymin=0, favicon_xmax=0, favicon_ymax=0;
	UWORD logo_ymin = 0, logo_xmin = 0, logo_ymax = 0, logo_xmax = 0;
	UWORD text_xmin, text_ymin, text_xmax, text_ymax;
	UWORD currentX, lastCharWidth, currentCharWidth, averageWidth, spaceWidth;
	UWORD border, right_border;
	unsigned char *currentChar = NULL;
	BOOL cont = TRUE, pause = FALSE, nowait = FALSE, inverse = FALSE;
	BOOL restore_logo_border = FALSE;
	BOOL restore_text_border = FALSE;
	BOOL restore_favicon_border = FALSE;
	Feed feed_ptr = NULL;
	Item feed_item = NULL;
	struct TextFont * UsedFont = NULL;
	UBYTE orig_Style = 0;
	long orange_pen = -1, winbrd_pen = -1;
	Object * dto = NULL, * favicon = NULL;
	ULONG last_favicon = 0;
	struct BitMap * SepImgBM = NULL;
	struct BitMapHeader * SepImgBMHD = NULL;
	APTR SepImgMask = NULL;
	
	ENTER();
	DBG("Window = %lx, RatPort = %lx, width = %ld, height = %ld\n",
		window, window->RPort, window->Width, window->Height );
	
	while((msg = GetMsg( window->UserPort)))
		ReplyMsg( msg);
	
	if( data->font_isTrueType )
	{
		ULONG a1 = 0, a2 = 0;
		
		TT_SetAttrs( rp,
			TT_Window,		(ULONG) window,
			//TT_Antialias,		TT_Antialias_On,
		TAG_DONE );
		
		TT_GetAttrs( rp,
			TT_FontHeight,		(ULONG) &a1,
			TT_FontBaseline,	(ULONG) &a2,
		TAG_DONE );
		
		textheight = a1 & 0xffff;
		bl = a2 & 0xffff;
		
		// may I'll best do this after checking the feed's encoding..?
		averageWidth = TT_TextLength( rp, "M", 1);
		averageWidth += TT_TextLength( rp, "W", 1);
		spaceWidth = TT_TextLength( rp, " ", 1);
	}
	else
	{
		UsedFont = rp->Font;
		orig_Style = UsedFont->tf_Style;
		textheight = UsedFont->tf_YSize;
		bl = UsedFont->tf_Baseline;
		
		averageWidth = TextLength( rp, "M", 1);
		averageWidth += TextLength( rp, "W", 1);
		spaceWidth = TextLength( rp, " ", 1);
	}
	averageWidth /= 2;
	y = (DISPLAY_HEIGHT / 2) - (textheight / 2);	/* Start for horizontal text output */
	scrollY = y;					/* ... and ditto for scroll */
	TextYAxis = (y += bl);				/* Real text start (X) */
	currentX = 0;					/* Offset in display */
	currentCharWidth = 0;				/* Width (in pixels) of current char */
	lastCharWidth = 0;
	scrollX = 1;
	border = right_border = 0;
	
	if( data->layout > 0 ) // settings for layout'ing
	{
		scrollX = NW_BOXWIDTH;
		border = NW_BORDER;
		
		if(!data->use_favicon || window->Height < (FAVICON_WIDTH + (border*2)))
		{
			right_border = border + 2;
		}
		else
		{
			right_border = border * 3 + FAVICON_WIDTH;
		}
	}
	DBG_VALUE(textheight);
	DBG_VALUE(averageWidth);
	DBG_VALUE(scrollY);
	
	// set the values of the REAL text printable area
	DISPLAY_WIDTH -= right_border;
	DISPLAY_HEIGHT -= border * 2;
	text_xmin = scrollX;
	text_ymin = scrollY;
	text_xmax = DISPLAY_WIDTH - 1;
	text_ymax = (scrollY + textheight);
	
	if( data->boxcolor == (UWORD) -1 )
		data->boxcolor = 4;
	
	if( data->layout > 0 )
	{
		UWORD bpc; // border box color
		ULONG r,g,b;
		
		bpc = data->boxcolor;
		
		// this is a borderless window, I'll paint some square on my own
		SetDrMd( rp, JAM2);
		SetDrPt( rp,(UWORD) ~0);
		
		// get Orange color
		r = MAKECOLOR32(255);
		g = MAKECOLOR32(160);
		b = MAKECOLOR32( 0 );
		orange_pen = ObtainBestPenA(window->WScreen->ViewPort.ColorMap,r,g,b,NULL);
		DBG_VALUE(orange_pen);
		
		// get window border color
		r = g = b = MAKECOLOR32(210);
		winbrd_pen = ObtainBestPenA(window->WScreen->ViewPort.ColorMap,r,g,b,NULL);
		DBG_VALUE(winbrd_pen);
		
		switch( data->layout )
		{
			case 2:
				SetAPen( rp, orange_pen );
				break;
			default:
			case 1:
				SetAPen( rp, bpc );
				break;
		}
		
		// top border
		RectFill( rp, 0, 0, window->Width, border );
		
		// left BOX
		RectFill( rp, 0, border, scrollX, window->Height - border );
		
		// right border
		RectFill( rp, window->Width - right_border, 0, window->Width, window->Height );
		
		// bottom border
		RectFill( rp, 0, window->Height - border, window->Width, window->Height );
		
		// draw a border box arround the visible area for the text
		RectFillOutline( rp, text_xmin, border+1, text_xmax+1, text_ymax+1, 1, 2 );
		DBG_ASSERT(text_xmin == scrollX);
		DBG_ASSERT(scrollX == NW_BOXWIDTH);
		DBG_ASSERT(text_ymin > border);
		text_xmin++;
		
		// window border
		//RectFillOutline( rp, 0, 0, window->Width, window->Height,bpc+1,1);
		RectFillOutline( rp, 0, 0, window->Width-1, window->Height-1, winbrd_pen, 1);
		
		// Draw the [RSS] logo
		{
			struct TextFont * font = NULL, * of = NULL;
			
			ly = y;
			logo_xmin = (NW_BOXWIDTH/2)-(RSSLOGO_WIDTH/2);
			logo_ymin = ((window->Height/2)-(RSSLOGO_HEIGHT/2))-1;
			logo_xmax = logo_xmin + RSSLOGO_WIDTH;
			logo_ymax = logo_ymin + RSSLOGO_HEIGHT;
			
			if( data->layout != 2 ) // the whole box is orange already
			{
				SetAPen( rp, orange_pen );
				RectFill( rp, logo_xmin, logo_ymin, logo_xmax, logo_ymax );
			}
			
			if((font = SystemFont ( 8 )))
			{
				of = UsedFont;
				
				SetFont( rp, font );
				
				ly = (RSSLOGO_HEIGHT/2)-(font->tf_YSize/2)+logo_ymin+font->tf_Baseline;
			}
			
			/**
			 * font SHOULD be != NULL (always, I guest), but if it
			 * is not avoid the RSS writing if we are using 
			 * TrueType fonts, because it will not fit probably...
			 */
			if( font || !data->font_isTrueType )
			{
				tlen = TextLength( rp, "RSS", 3);
				TextShadow( rp, (RSSLOGO_WIDTH/2)-(tlen/2)+logo_xmin, ly, "RSS", 3, 2, 1, orange_pen);
			}
			
			// draw logo border
			if( data->layout != 2 ) // i dont want it for layout #2
				RectFillOutline( rp, logo_xmin, logo_ymin, logo_xmax, logo_ymax, 2, 1 );
			
			if( font )
			{
				if( data->font_isTrueType )
					TT_SetFont( rp, data->font );
				else
					SetFont( rp, of );
				CloseFont( font );
			}
		}
		
		// draw space to favicon
		favicon_xmin = window->Width - right_border + border + (border/2);
		favicon_ymin = ((window->Height/2)-(FAVICON_HEIGHT/2))-1;
		favicon_xmax = favicon_xmin+FAVICON_WIDTH;
		favicon_ymax = favicon_ymin+FAVICON_WIDTH;
		RectFillOutline( rp, favicon_xmin, favicon_ymin, favicon_xmax, favicon_ymax, 1,2);
	}
	
	DBG_ASSERT(textheight < DISPLAY_HEIGHT);
	DBG_AXIS(text);
	DBG_AXIS(logo);
	DBG_AXIS(favicon);
	
	if(data->text_pen == (UWORD)-1)
		data->text_pen = 1 + (1L << 7);
	SetDrMd( rp, JAM1);
	SetAPen( rp, data->text_pen );
	
	if( data->bgcolor != (UWORD)-1 )
	{
		SetBPen( rp, data->bgcolor );
		
		//RectFill( rp, scrollX + 1, border + 1, DISPLAY_WIDTH - 1, DISPLAY_HEIGHT - 1);
		RectFill( rp, text_xmin, text_ymin, text_xmax, text_ymax );
	}
	
	if( data->sepimg != NULL )
	{
		// load image used to separate news items
		
		dto = NewDTObject( data->sepimg,
			DTA_GroupID, GID_PICTURE,
			PDTA_Screen, (ULONG) window->WScreen,
		TAG_DONE );
		
		if( dto )
		{
			//DoDTMethod(dto,NULL,NULL,DTM_PROCLAYOUT,NULL,TRUE);
			DoMethod( dto, DTM_PROCLAYOUT, NULL, 1L);
			
			GetDTAttrs( dto,
				PDTA_BitMapHeader,	(ULONG) &SepImgBMHD,
				PDTA_DestBitMap,	(ULONG) &SepImgBM,
				PDTA_MaskPlane,		(ULONG) &SepImgMask,
			TAG_DONE );
			
			if( SepImgBMHD->bmh_Height > textheight )
			{
				// image will not fit on window
				// TODO: scale it (?)
				DisposeDTObject( dto );
				dto = NULL;
				SepImgBM = NULL;
				tell("Image \"%s\" do not fit into window\n", FilePart( data->sepimg ));
			}
			else
			{
				if( ! SepImgBM ) // this is needed?...
				{
					DBG("Falling back to PDTA_BitMap ----------\n");
					
					GetDTAttrs(dto, PDTA_BitMap, (ULONG) &SepImgBM, TAG_DONE);
				}
			}
		}
	}
	
	if( ! data->delay )
		data->delay = 2;
	
	do {
		if((msg = GetMsg( window->UserPort )))
		{
			struct IntuiMessage * imsg = (struct IntuiMessage *)msg;
			USHORT code;
			SHORT mousex, mousey;
			ULONG class;
			
			class  = imsg->Class;
			code   = imsg->Code;
			mousex = imsg->MouseX;
			mousey = imsg->MouseY;
			
			cont = !cont;	// Toggle scroll-status
			
			switch( class )
			{
				case IDCMP_MOUSEBUTTONS:
				{
					switch( code )
					{
						case SELECTDOWN:
						{
							if(_isinarea(logo,mousex,mousey))
							{
								if( data->layout != 2 ) // i dont want it for layout #2
								{
									RectFillOutline( rp, logo_xmin, logo_ymin, logo_xmax, logo_ymax, 1,2);
									restore_logo_border = TRUE;
								}
							}
							else if(_isinarea(text,mousex,mousey))
							{
								RectFillOutline( rp, text_xmin-1, border+1, text_xmax+1, text_ymax+1, 2, 2 );
								restore_text_border = TRUE;
							}
							else if(_isinarea(favicon,mousex,mousey))
							{
								RectFillOutline( rp, favicon_xmin, favicon_ymin, favicon_xmax, favicon_ymax, 1,2);
								restore_favicon_border = TRUE;
							}
						}	break;
						case SELECTUP:
						case MENUUP:
						{
							if( restore_logo_border )
							{
								restore_logo_border = FALSE;
								RectFillOutline( rp, logo_xmin, logo_ymin, logo_xmax, logo_ymax, 2, 1 );
								
								About ( ) ;
							}
							else if( restore_text_border )
							{
								restore_text_border = FALSE;
								RectFillOutline( rp, text_xmin-1, border+1, text_xmax+1, text_ymax+1, ((data->layout > 0) ? 1 : data->bgcolor), ((data->layout > 0) ? 2 : data->bgcolor));
							}
							else if( restore_favicon_border )
							{
								restore_favicon_border = FALSE;
								RectFillOutline( rp, favicon_xmin, favicon_ymin, favicon_xmax, favicon_ymax, 2,1);
							}
							
							if(_isinarea(favicon,mousex,mousey) && feed_ptr)
							{
								struct Library * OpenURLBase;
								
								if((OpenURLBase = OpenLibrary( __openurlname, 4 )))
								{
									URL_OpenA( feed_ptr->hostname, NULL );
									CloseLibrary( OpenURLBase );
								}
								else DisplayBeep(NULL);
							}
							else if(_isinarea(text,mousex,mousey) && feed_item)
							{
								STATIC STRPTR last_click = NULL;
								
								if(feed_item != NULL && feed_item->link != NULL && feed_item->link != last_click)
								{
									struct Library * OpenURLBase;
									
									RectFillOutline( rp, logo_xmin, logo_ymin, logo_xmax, logo_ymax, 1,2);
									WaitTOF();
									RectFillOutline( rp, logo_xmin, logo_ymin, logo_xmax, logo_ymax, 2,1);
									WaitTOF();
									
									if((OpenURLBase = OpenLibrary( __openurlname, 4 )))
									{
										URL_OpenA( feed_item->link, NULL );
										CloseLibrary( OpenURLBase );
										last_click = feed_item->link;
									}
									else DisplayBeep(NULL);
									
									RectFillOutline( rp, logo_xmin, logo_ymin, logo_xmax, logo_ymax, 1,2);
									WaitTOF();
									RectFillOutline( rp, logo_xmin, logo_ymin, logo_xmax, logo_ymax, 2,1);
									WaitTOF();
								}
							}
							cont = TRUE;
							break;
						}
						default:
							break;
					}
					ReplyMsg( msg);	/* So, reply it */
					msg = NULL;	/* Say, that wont force us to quit */
					Delay( 2);	/* Take a short timeout */
				}	break;
				
				case IDCMP_RAWKEY:
				{
					switch( code )
					{
						//case 0x4C: /* up arrow */
						//case 0x4D: /* down arrow */
						case 0x4F: /* left arrow (down) */
							nowait = TRUE;
							cont = TRUE;
							break;
						case 0xCF: /* left arrow (up) */
							nowait = FALSE;
							cont = TRUE;
							break;
						#if 0
						// TODO: ->item linked list needs a ->prev field
						case 0x4E: /* right arrow (down) */
						case 0xCE: /* right arrow (up) */
							if(!(inverse = !inverse))
								inverse_new = TRUE;
							cont = TRUE;
							break;
						#endif
						//case 0x40: /* space (down) */
						case 0xc0: /* space (up) */
							pause = !pause;
							cont = TRUE;
							break;
						default:
							break;
					}
					if( code != 0x45 ) // 0x45 == ESC key
					{
						ReplyMsg( msg );
						msg = NULL;
					}
				}	break;
				
				default:
					break;
			}
		}
		
		if( pause || nowait < 1 || currentChar == NULL )
		{
			ULONG wSig = SetSignal(0L,0L);
			
			if(wSig & data->mPortSig)	mPortDispatch( );
			if(wSig & SIGBREAKF_CTRL_C)	break;
			
			//Delay(data->delay + (cont ? 0 : 4));
			
			if( cont == FALSE || pause )
			{
				Delay( 20 );
				continue;
			}
		}
		
		if( currentChar == NULL )
		{
			ObtainSemaphore(sem);
			
			if( feed_ptr == NULL )
				feed_ptr = data->feeds;
			
			if( feed_item == NULL )
			{
				if((feed_item = feed_ptr->item))
				{
					if( data->font_isTrueType )
					{
						// new feed, set encoding if needed
						ULONG tt_enc = TT_Encoding_Default;
						
						switch( feed_ptr->enc )
						{
							case FE_UTF8:
								tt_enc = TT_Encoding_UTF8;
								break;
							//case FE_UTF16:
							case FE_UTF16BE:
								tt_enc = TT_Encoding_UTF16_BE;
								break;
							case FE_UTF16LE:
								tt_enc = TT_Encoding_UTF16_LE;
								break;
							//case FE_UTF32:
							case FE_UTF32BE:
								tt_enc = TT_Encoding_UTF32_BE;
								break;
							case FE_UTF32LE:
								tt_enc = TT_Encoding_UTF32_LE;
								break;
							case FE_NONE:
							default:
								tt_enc = TT_Encoding_Default;
								break;
						}
						DBG_VALUE(tt_enc);
						
						TT_SetAttrs( rp, TT_Encoding, tt_enc, TAG_DONE);
					}
				}
			}
			
			if(feed_item != NULL) // does the subtask filled something?
			{
				UBYTE ScreenTitle[200], * src, * dst;
				LONG len, mlen = sizeof(ScreenTitle);
				
				currentChar = feed_item->text;
				len = snprintf( ScreenTitle, sizeof(ScreenTitle)-1,
					"[%s] %s - ", _PROGRAM_NAME, feed_ptr->hostname );
				
				src = feed_item->text;
				dst = &ScreenTitle[len];
				mlen -= len;
				while( *src && *src < 31 ) src++;
				while( *src && *src != ':' && --mlen > 0 )
					*dst++ = *src++;
				*dst = 0;
				
				SetWindowTitles( window, NULL, ScreenTitle );
				
				if((feed_item = feed_item->next) == NULL)
					feed_ptr = feed_ptr->next;
			}
			ReleaseSemaphore(sem);
			
			if( currentChar == NULL )
			{
				Delay( 20 );
				continue;
			}
		}
		
		if (!currentCharWidth)
		{
			while( *currentChar > 0 && *currentChar < 31 )
			{
				switch( *currentChar )
				{
					case EIF_START:
						//DBG(" ---- EIF_START\n");
						if( feed_ptr && data->use_favicon && !feed_ptr->favicon_error && *feed_ptr->favicon && last_favicon != (ULONG) feed_ptr )
						{
							last_favicon = (ULONG) feed_ptr;
							
							if( favicon )
								DisposeDTObject( favicon );
							
							favicon = NewDTObject( feed_ptr->favicon,
								DTA_GroupID, GID_PICTURE,
								PDTA_Screen, (ULONG) window->WScreen,
							TAG_DONE );
							DBG_POINTER(favicon);
							
							if( favicon )
							{
								APTR mask = NULL;
								struct BitMap * bm = NULL;
								
								DoMethod( favicon, DTM_PROCLAYOUT, NULL, 1L);
								
								GetDTAttrs( favicon,
									PDTA_DestBitMap,(ULONG) &bm,
									PDTA_MaskPlane, (ULONG) &mask,
								TAG_DONE );
								DBG_POINTER(mask);
								DBG_POINTER(bm);
								
								if(!bm)
									GetDTAttrs(favicon, PDTA_BitMap, (ULONG) &bm, TAG_DONE);
								
								if( mask != NULL )
								{
									BltMaskBitMapRastPort( bm, 0, 0, rp,
										favicon_xmin,
										favicon_ymin,
										FAVICON_WIDTH,
										FAVICON_HEIGHT, ABC|ABNC|ANBC, mask );
								}
								else
								{
									BltBitMapRastPort( bm, 0, 0, rp,
										favicon_xmin,
										favicon_ymin,
										FAVICON_WIDTH,
										FAVICON_HEIGHT, 0xc0 );
								}
								
								// draw border around the logo
								RectFillOutline( rp, favicon_xmin, favicon_ymin, favicon_xmax, favicon_ymax, 2,1);
							}
							else
							{
								struct TextFont * font, * of = NULL;
								
								feed_ptr->favicon_error = 1;
								
								if((font = SystemFont ( 10 )))
								{
									of = UsedFont;
									SetFont( rp, font );
									
									ly = (FAVICON_HEIGHT/2)-(font->tf_YSize/2)+favicon_ymin+font->tf_Baseline;
									
									tlen = TextLength( rp, "?", 1);
									TextShadow( rp, (FAVICON_WIDTH/2)-(tlen/2)+favicon_xmin, ly, "?", 1, data->boxcolor, 1, 2);
									
									if( data->font_isTrueType )
										TT_SetFont( rp, data->font );
									else
										SetFont( rp, of );
									CloseFont( font );
								}
								else RectFill( rp, favicon_xmin, favicon_ymin, favicon_xmax, favicon_ymax );
							}
						}
					/*	else
						{
							DBG_VALUE(data->use_favicon);
							DBG_VALUE(feed_ptr->favicon_error);
							DBG_STRING(feed_ptr->favicon);
							DBG_ASSERT(last_favicon != feed_ptr);
						}
					*/	break;
					case EIF_END:
						//DBG(" ---- EIF_END\n");
						if( SepImgBM != NULL )
						{
							if( SepImgMask != NULL )
							{
								BltMaskBitMapRastPort(SepImgBM,0,0, rp,
									DISPLAY_WIDTH - SepImgBMHD->bmh_Width,
									((window->Height/2)-(SepImgBMHD->bmh_Height/2)),
									SepImgBMHD->bmh_Width,
									SepImgBMHD->bmh_Height, ABC|ABNC|ANBC, SepImgMask );
							}
							else
							{
								BltBitMapRastPort(SepImgBM,0,0, rp,
									DISPLAY_WIDTH - SepImgBMHD->bmh_Width,
									((window->Height/2)-(SepImgBMHD->bmh_Height/2)),
									SepImgBMHD->bmh_Width,
									SepImgBMHD->bmh_Height, 0xc0);
							}
							currentCharWidth = SepImgBMHD->bmh_Width + (FEEDSP*spaceWidth);
						}
						else
						{
							// no space to fill image, use a character
							*currentChar = '·';
							continue;
						}
						break;
					case EIF_TEXT_BOLD:
						//DBG(" ---- EIF_TEXT_BOLD\n");
						if( data->font_isTrueType )
						{
						}
						else
						{
							UsedFont->tf_Style |= FSF_BOLD;
							SetSoftStyle( rp, UsedFont->tf_Style, 7);
						}
						break;
					case EIF_TEXT_NORMAL:
						//DBG(" ---- EIF_TEXT_NORMAL\n");
						if( data->font_isTrueType )
						{
						}
						else
						{
							SetSoftStyle( rp, orig_Style, 7);
						}
						break;
				}
				currentChar++;
			}
		}
		if (!currentCharWidth) /* Current character worked out ? */
		{
			 /* No next character?, or (at least) TheRegister sets 0xE280.. at description end */
			if(!*currentChar)
			{
				/* Restart text output only when the complete display-row contains no text ! */
			//	if (currentX == 0 || currentX == DISPLAY_WIDTH - 1)
				{
					currentChar = NULL;
			//		currentX = 0;
				}
			}
			else  /* Next char to print */
			{
				Move( rp, DISPLAY_WIDTH - averageWidth - 1, y );
				if( data->font_isTrueType )
				{
					currentCharWidth = TT_TextLength( rp, currentChar, 1);
					TT_Text( rp, currentChar, 1);
				}
				else
				{
					currentCharWidth = TextLength( rp, currentChar, 1);
					Text( rp, currentChar, 1);
				}
				currentChar ++;
			}
		}
		
		/* Scroll the text one pixel to the left */
		ScrollRaster( rp, 1, 0, text_xmin, text_ymin, text_xmax, text_ymax );
		
		/* Keep track of display width */
	//	if (++currentX == DISPLAY_WIDTH - border - 1)
	//		currentX = 0;
		
		/* One less to work out */
		if (currentCharWidth)
			currentCharWidth --;
		
		if( nowait == 0 )
			WaitTOF();
		
	} while( msg == NULL );
	
	if ( msg )
	{
		ReplyMsg( msg);
		while ((msg = GetMsg( window->UserPort)))
			ReplyMsg( msg);
	}
	
	if( orange_pen != -1 )
		ReleasePen( window->WScreen->ViewPort.ColorMap, orange_pen );
	if( winbrd_pen != -1 )
		ReleasePen( window->WScreen->ViewPort.ColorMap, winbrd_pen );
	
	if( dto != NULL )
		DisposeDTObject ( dto ) ;
	if( favicon != NULL )
		DisposeDTObject( favicon );
	
	LEAVE();
}

/*****************************************************************************/

INLINE LONG __main_transport ( struct WBStartup * _WBenchMsg )
{
	LONG rc = RETURN_FAIL;
	LONG saved_ioerr = 0;
	
	ENTER();
	
	if((data->mPort = CreateMsgPort ()))
	{
		APTR msg;
		BYTE pri = 0;
		
		data->mPortSig = (1L << data->mPort->mp_SigBit);
		data->task = FindTask(NULL);
		
		if( data->task_pri != 0 )
			pri = SetTaskPri( data->task, data->task_pri );
		
		if(CreateSubTask( ))
		{
			DBG("Everything went ok, starting scroll job...\n");
			
			__main_loop ( _WBenchMsg );
			rc = RETURN_OK;
		}
		else saved_ioerr = IoErr();
		
		if( data->task_pri != 0 )
			SetTaskPri( data->task, pri );
		
		// is there running childs/threads/subprocesses?
		if(data->child != NULL)
		{
			DBG("****Breaking child...\n");
			
			Signal( data->child, SIGBREAKF_CTRL_C );
			
			do {
				ULONG wSig = SetSignal(0L,0L);
				
				if(wSig & data->mPortSig)
					mPortDispatch( );
				if(wSig & SIGBREAKF_CTRL_E)
					break;
				
				Delay( TICKS_PER_SECOND / 2 );
				
			} while(1);
		}
		
		// clean and remove message port
		while((msg=GetMsg( data->mPort )))
			ReplyMsg( msg );
		DeleteMsgPort( data->mPort );
	}
	
	if( saved_ioerr == 0 )
		saved_ioerr = IoErr();
	SetIoErr( saved_ioerr );
	
	DBG_VALUE(saved_ioerr);
	LEAVE();
	
	return rc;
}

LONG __main_interface ( struct WBStartup * _WBenchMsg )
{
	LONG rc = RETURN_FAIL;
	LONG saved_ioerr = 0;
	
	ENTER();
	
	if((sem = __create_semaphore ( )))
	{
		if( SetData ( _WBenchMsg ))
		{
			if( IsXml2Available ( ))
			{
				if( CreateWindow ( ))
				{
					rc = __main_transport ( _WBenchMsg );
					saved_ioerr = IoErr();
					
					DeleteWindow ( ) ;
				}
				else rc = RETURN_OK; // a requester  was showed
			}
			else rc = RETURN_OK; // a requester  was showed
			
			if( TTEngineBase != NULL )
				CloseLibrary( TTEngineBase );
			
			FreeData ( ) ;
		}
		else saved_ioerr = IoErr();
		
		__delete_semaphore( sem );
	}
	
	DBG_VALUE(rc);
	
	if( rc != RETURN_OK )
	{
		char buffer[82];
		
		if( saved_ioerr == 0 )
			saved_ioerr = IoErr();
		DBG_VALUE(saved_ioerr);
		
		Fault( saved_ioerr, "internal error", buffer, sizeof(buffer) - 1 );
		
		tell( buffer );
	}
	
	#ifndef DEBUG
	RemoveTempFiles ( ) ;
	#endif
	
	LEAVE();
	
	return(rc);
}
