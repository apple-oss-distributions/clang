/*
 *  CFUtils.h
 *  dwarfutils
 *
 *  Created by Greg Clayton on 3/5/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef __CFUtils_h__
#define __CFUtils_h__

#include <CoreFoundation/CoreFoundation.h>

#ifdef __cplusplus

#include <assert.h>

//----------------------------------------------------------------------
// Templatized CF helper class that can own any CF pointer and will
// call CFRelease() on any valid pointer it owns unless that pointer is
// explicitly released using the release() member function. This class
// is designed to mimic the std::auto_ptr<T> class and has all of the
// same functions. The one thing to watch out for is the 
// CFReleaser<T>::release() function won't actually CFRelease any owned
// pointer, it is designed to relinquish ownwership of the pointer just
// like std:auto_ptr<T>::release() does.
//----------------------------------------------------------------------
template <class T>
class CFReleaser
{
public:
	//----------------------------------------------------------
	// Constructor that takes a pointer to a CF object that is 
	// to be released when this object goes out of scope
	//----------------------------------------------------------
	CFReleaser(T ptr = NULL) : 
		_ptr(ptr) 
	{
	}

	//----------------------------------------------------------
	// The copy constructor will retain the pointer contained in
	// the RHS if it contains a valid pointer value
	//----------------------------------------------------------
	CFReleaser(const CFReleaser& rhs) : 
		_ptr(rhs.get())
	{
		if (get())
			::CFRetain(get());
	}
	
	
	//----------------------------------------------------------
	// The destructor will release the pointer that it contains
	// if it has a valid pointer.
	//----------------------------------------------------------
	virtual ~CFReleaser() 
	{
		reset(); 
	}
	
	//----------------------------------------------------------
	// Assignments
	//----------------------------------------------------------
	CFReleaser& 
	operator= (const CFReleaser<T>& rhs) 
	{
		if (this != &rhs)
		{
			// Replace our owned pointer with the new one
			reset(rhs.get());
			// Retain the current pointer that we own
			if (get())
				::CFRetain(get());
		}
		return *this;
	}

	//----------------------------------------------------------
	// Get the address of the contained type in case it needs
	// to be passed to a function that will fill in a pointer
	// value. The function currently will assert if _ptr is not
	// NULL because the only time this method should be used is
	// if another function will modify the contents, and we 
	// could leak a pointer if this is not NULL. If the 
	// assertion fires, check the offending code, or call 
	// reset() prior to using the "ptr_address()" member to make
	// sure any owned objects has CFRelease called on it.
	//----------------------------------------------------------
	T*		
	ptr_address()	
	{
		assert (_ptr == NULL);
		return &_ptr;	
	}
	
	//----------------------------------------------------------
	// Access the pointer itself
	//----------------------------------------------------------
	T
	get()
	{
		return _ptr;		
	}

	const T
	get() const
	{
		return _ptr;		
	}


	//----------------------------------------------------------
	// Set a new value for the pointer and CFRelease our old
	// value if we had a valid one. 
	//----------------------------------------------------------
	void	
	reset(T ptr = NULL)
	{
		if ((_ptr != NULL) && (ptr != _ptr))
			::CFRelease(_ptr); 
		_ptr = ptr;
	}

	//----------------------------------------------------------
	// Release ownership without calling CFRelease. This class
	// is designed to mimic std::auto_ptr<T>, so the release
	// method releases ownership of the contained pointer 
	// without calling CFRelease.
	//----------------------------------------------------------
	T	
	release() 
	{
		T tmp = _ptr; 
		_ptr = NULL; 
		return tmp; 
	}

private:
	T _ptr;
};

#endif	// #ifdef __cplusplus
#endif	// #ifndef __CFUtils_h__

