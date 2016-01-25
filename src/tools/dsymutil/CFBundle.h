//----------------------------------------------------------------------
//  CFBundle.h
//  debugserver
//
//  Created by Greg Clayton on 1/16/08.
//  Copyright (c) 2005-2011 Apple Inc. All rights reserved.
//
//----------------------------------------------------------------------

#ifndef __CFBundle_h__
#define __CFBundle_h__

#include "CFUtils.h"

class CFBundle : public CFReleaser<CFBundleRef>
{
public:
	//------------------------------------------------------------------
	// Constructors and Destructors
	//------------------------------------------------------------------
	CFBundle(const char *path = NULL);
	CFBundle(const CFBundle& rhs);
	CFBundle(CFURLRef url);
	CFBundle& operator=(const CFBundle& rhs);
	virtual ~CFBundle();
        
	bool SetPath (const char *path);
	CFStringRef GetIdentifier () const;
	CFTypeRef GetValueForInfoDictionaryKey(CFStringRef key) const;

};

#endif // #ifndef __CFBundle_h__
