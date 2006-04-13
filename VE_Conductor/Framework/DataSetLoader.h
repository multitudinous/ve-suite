/////////////////////////////////////////////////////////////////////////////
// Name:        DataSetLoader.h
// Purpose:     
// Author:      
// Modified by: 
// Created:     11/04/2006 20:55:38
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

#ifndef _DATASETLOADER_H_
#define _DATASETLOADER_H_

#if defined(__GNUG__) && !defined(NO_GCC_PRAGMA)
#pragma interface "DataSetLoader.h"
#endif

/*!
 * Includes
 */

////@begin includes
#include "wx/image.h"
////@end includes

/*!
 * Forward declarations
 */

////@begin forward declarations
////@end forward declarations

/*!
 * Control identifiers
 */

////@begin control identifiers
////@end control identifiers

/*!
 * DataSetLoader class declaration
 */

class DataSetLoader: public wxApp
{    
    DECLARE_CLASS( DataSetLoader )
    DECLARE_EVENT_TABLE()

public:
    /// Constructor
    DataSetLoader();

    /// Initialises the application
    virtual bool OnInit();

    /// Called on exit
    virtual int OnExit();

////@begin DataSetLoader event handler declarations
////@end DataSetLoader event handler declarations

////@begin DataSetLoader member function declarations
////@end DataSetLoader member function declarations

////@begin DataSetLoader member variables
////@end DataSetLoader member variables
};

/*!
 * Application instance declaration 
 */

////@begin declare app
DECLARE_APP(DataSetLoader)
////@end declare app

#endif
    // _DATASETLOADER_H_
