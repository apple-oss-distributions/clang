//----------------------------------------------------------------------
//  CFString.h
//  debugserver
//
//  Created by Greg Clayton on 1/16/08.
//  Copyright (c) 2005-2011 Apple Inc. All rights reserved.
//
//----------------------------------------------------------------------

#ifndef __CFString_h__
#define __CFString_h__

#include "CFUtils.h"
#include <iosfwd>

class CFString : public CFReleaser<CFStringRef>
{
public:
	//------------------------------------------------------------------
	// Constructors and Destructors
	//------------------------------------------------------------------
                        CFString (CFStringRef cf_str = NULL);
						CFString (const char *s, CFStringEncoding encoding);
                        CFString (const CFString& rhs);
                        CFString& operator= (const CFString& rhs);
                        virtual ~CFString ();

        const char *    GetFileSystemRepresentation (std::string& str);
        CFStringRef     SetFileSystemRepresentation (const char *path);
        CFStringRef     SetFileSystemRepresentationFromCFType (CFTypeRef cf_type);
		CFStringRef		SetFileSystemRepresentationAndExpandTilde (const char *path);
        const char *    UTF8 (std::string& str);
        CFIndex         GetLength() const;
		static const char *UTF8 (CFStringRef cf_str, std::string& str);
		static const char *FileSystemRepresentation (CFStringRef cf_str, std::string& str);

};

#endif // #ifndef __CFString_h__
