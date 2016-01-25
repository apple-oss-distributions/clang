/*
 *  CFStringUtils.h
 *  dwarfutils
 *
 *  Created by Greg Clayton on 1/24/07.
 *  Copyright (c) 2007-2011 Apple Inc. All rights reserved.
 *
 */

#ifndef __CFStringUtils_h__
#define __CFStringUtils_h__

#include <CoreFoundation/CoreFoundation.h>

#ifdef __cplusplus
extern "C" {
#endif

char* CFStringToFileSystemRepresentation (CFStringRef str, Boolean trim);
char* CFStringToUTF8 (CFStringRef str, Boolean trim);

#ifdef __cplusplus
}
#endif

#endif
