//----------------------------------------------------------------------
//  CFBundle.cpp
//  debugserver
//
//  Created by Greg Clayton on 1/16/08.
//  Copyright (c) 2005-2011 Apple Inc. All rights reserved.
//
//----------------------------------------------------------------------

#include "CFBundle.h"
#include "CFString.h"

//----------------------------------------------------------------------
// CFBundle constructor
//----------------------------------------------------------------------
CFBundle::CFBundle(const char *path) :
    CFReleaser<CFBundleRef>()
{
    if (path && path[0])
        SetPath(path);
}

//----------------------------------------------------------------------
// CFBundle copy constructor
//----------------------------------------------------------------------
CFBundle::CFBundle(const CFBundle& rhs) :
    CFReleaser<CFBundleRef>(rhs)
{
	
}


CFBundle::CFBundle(CFURLRef url) :
    CFReleaser<CFBundleRef>(url ? CFBundleCreate(NULL, url) : NULL)
{
}


//----------------------------------------------------------------------
// CFBundle copy constructor
//----------------------------------------------------------------------
CFBundle& 
CFBundle::operator=(const CFBundle& rhs)
{
	if (this != &rhs)
		*this = rhs;
    return *this;
}

//----------------------------------------------------------------------
// Destructor
//----------------------------------------------------------------------
CFBundle::~CFBundle()
{
}

//----------------------------------------------------------------------
// Set the path for a bundle by supplying a 
//----------------------------------------------------------------------
bool
CFBundle::SetPath (const char *in_path)
{
    // Release our old bundle and ULR
    reset();    // This class is a CFReleaser<CFBundleRef>
    
	if (in_path && in_path[0])
	{
		char resolved_path[PATH_MAX];
		const char *path = ::realpath(in_path, resolved_path);
		if (path == NULL)
			path = in_path;

		CFAllocatorRef alloc = kCFAllocatorDefault;
        // Make our Bundle URL
		CFReleaser<CFURLRef> bundle_url(::CFURLCreateFromFileSystemRepresentation (alloc, (const UInt8 *)path, strlen (path), false));
        if (bundle_url.get())
        {
			CFIndex last_length = LONG_MAX;
			
			while (bundle_url.get() != NULL)
			{
				// Check the Path range and make sure we didn't make it to just
				// "/", ".", or ".."
				CFRange rangeIncludingSeparators;
				CFRange range = ::CFURLGetByteRangeForComponent(bundle_url.get(), kCFURLComponentPath, &rangeIncludingSeparators);
				if (range.length > last_length)
					break;

				reset (::CFBundleCreate (alloc, bundle_url.get()));
				if (get() != NULL)
				{
					if (GetIdentifier() != NULL)
						break;
					reset();
				}
				bundle_url.reset (::CFURLCreateCopyDeletingLastPathComponent(alloc, bundle_url.get()));
				
				last_length = range.length;
			}
        }
    }
    return get() != NULL;
}

CFStringRef
CFBundle::GetIdentifier () const
{
    CFBundleRef bundle = get();
    if (bundle != NULL)
        return ::CFBundleGetIdentifier (bundle);
    return NULL;
}

CFTypeRef
CFBundle::GetValueForInfoDictionaryKey(CFStringRef key) const
{
    CFBundleRef bundle = get();
    if (bundle != NULL)
        return ::CFBundleGetValueForInfoDictionaryKey(bundle, key);
    return NULL;
}


