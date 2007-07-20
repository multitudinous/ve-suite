/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VE_CONDUCTOR_GUI_UTILITIES_TAG_
#define _VE_CONDUCTOR_GUI_UTILITIES_TAG_
/*!\file Tag.h
Tag API
*/
/*!\class Tag
*
*/
#include "VE_Conductor/Utilities/Polygon.h"
#include "VE_Conductor/Utilities/Link.h"
#include "VE_Installer/include/VEConfig.h"

#include "VE_Open/XML/Model/TagPtr.h"

#include <wx/gdicmn.h>
#include <wx/string.h>
#include <wx/dc.h>
#include <wx/event.h>

class wxScrolledWindow;

namespace VE_Conductor
{
namespace GUI_Utilities
{
class VE_CONDUCTOR_UTILS_EXPORTS Tag : public wxEvtHandler
{
public:
    ///Constructor
    Tag( wxScrolledWindow* designCanvas );
    ///Destructor
    ~Tag( void );
    ///Copy Constructor
    Tag( const Tag& );
    ///equal operator
    Tag& operator= ( const Tag& );

    ///Get the i'th point for the tag
    ///\param i The i'th point you are after
    wxPoint* GetConnectorsPoint( size_t i );
    ///Get tag text
    wxString* GetTagText( void );
    ///Get the polygon that will be rendered
    Polygon* GetPolygon( void );
    ///Get the bounging box for the tag
    wxRect* GetBoundingBox( void );
    ///Set the raw xml data to configure the tag
    ///\param inputTag The pointer to the class holding the tag data
    void SetVETagPtr( VE_XML::VE_Model::TagPtr inputTag );
    ///Get the raw xml data to be written back out
    VE_XML::VE_Model::TagPtr GetVETagPtr();
    ///Calculate tag polygon to be drawn
    void CalcTagPoly( void );
    ///Draw functions
    void DrawTagCon( bool flag, std::pair< double, double > scale );
    ///Draw functions
    void DrawTag( bool flag, wxDC& dc, std::pair< double, double > scale );

private:
    ///2 connectors for a tag, end and middle
    wxPoint cons[2]; 
    ///Text to be dispalyed in the tag
    wxString text;
    ///Box that the tag is contained in
    wxRect box;
    ///Poly is the current poly on the canvas
    Polygon poly;
    ///The canvas to be drawn to
    wxScrolledWindow* canvas;
    ///Unique identifier for this tag
    std::string uuid;
    ///Convert unicode strings
    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( 
            static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }
};
}
}
#endif
