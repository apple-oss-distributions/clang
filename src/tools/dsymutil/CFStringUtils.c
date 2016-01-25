/*
 *  CFStringUtils.c
 *  dwarfutils
 *
 *  Created by Greg Clayton on 1/24/07.
 *  Copyright (c) 2007-2011 Apple Inc. All rights reserved.
 *
 */

#include "CFStringUtils.h"

//----------------------------------------------------------------------
// Convert a CFString to a path suitable for posix functions. If TRIM
// is non-zero, trim the allocated buffer down as small as possible 
// after the conversion since more bytes will be allocated than needed.
// If the string has a short lifespan, then TRIM doesn't need to be set. 
// Returns a valid file system representation for the CFString, or NULL
// on failure. The caller is responsible for free'ing the result if it
// is not NULL.
//----------------------------------------------------------------------
char*
CFStringToFileSystemRepresentation (CFStringRef str, Boolean trim)
{
	char* path = NULL;
	if (str)
	{
		CFIndex max_path_len = CFStringGetMaximumSizeOfFileSystemRepresentation (str);
		if (max_path_len > 0)
		{
			path = (char *)malloc (max_path_len);
			if (path != NULL)
			{
				if (CFStringGetFileSystemRepresentation (str, path, max_path_len))
				{
					// Conversion succeeded, trim the buffer down if requested
					if (trim)
						path = (char *)realloc (path, strlen(path)+1);
				}
				else
				{
					// Conversion failed
					free (path);
					path = NULL;
				}
			}
		}
	}
	return path;
}

//----------------------------------------------------------------------
// Convert a CFString toUTF8. If TRIM is non-zero, trim the allocated 
// buffer down as small as possible after the conversion since more 
// bytes will be allocated than needed. If the resulting string has a 
// short lifespan, then TRIM doesn't need to be set.  Returns a UTF8 
// string pointer for the CFString, or NULL on failure. The caller is
// responsible for free'ing the result if it is not NULL.
//----------------------------------------------------------------------
char*
CFStringToUTF8 (CFStringRef str, Boolean trim)
{
	char *utf8_str = NULL;
	if (str)
	{
		const CFStringEncoding encoding = kCFStringEncodingUTF8;
		CFIndex max_utf8_str_len = CFStringGetLength (str);
		max_utf8_str_len = CFStringGetMaximumSizeForEncoding (max_utf8_str_len, encoding);
		if (max_utf8_str_len > 0)
		{
			utf8_str = (char *)malloc(max_utf8_str_len);
			if (utf8_str)
			{
				if (CFStringGetCString (str, utf8_str, max_utf8_str_len, encoding))
				{
					// Conversion succeeded, trim the buffer down if requested
					if (trim)
						utf8_str = (char *)realloc (utf8_str, strlen(utf8_str)+1);
				}
				else
				{
					// Conversion failed
					free (utf8_str);
					utf8_str = NULL;
				}
			}
	    }
    }
	return utf8_str;
}
