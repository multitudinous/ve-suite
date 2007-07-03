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
#ifndef _VE_CONDUCTOR_GUI_UTILITIES_LINK_
#define _VE_CONDUCTOR_GUI_UTILITIES_LINK_
/*!\file Link.h
Link API
*/
/*!\class VE_Conductor::GUI_Utilities::Link
*
*/
#include <vector>
#include <string>
#include <utility>

#include "VE_Installer/include/VEConfig.h"
#include "VE_Conductor/Utilities/Polygon.h"

#include <wx/gdicmn.h>
#include <wx/dc.h>
#include <wx/event.h>

class wxScrolledWindow;

namespace VE_Conductor
{
namespace GUI_Utilities
{
class VE_CONDUCTOR_UTILS_EXPORTS Link : public wxEvtHandler
{
public:
   ///Constructor
   Link( wxScrolledWindow* designCanvas );
   ///Destructor
   ~Link( void );
   ///Copy Constructor
   Link( const Link& );
   ///equal operator
   Link& operator= ( const Link& );
   ///Test for equality operator
   friend inline bool operator== ( const Link& l1, const Link& l2 )
   {
      if ( 
            (l1.Fr_mod == l2.Fr_mod) &&
            (l1.To_mod == l2.To_mod) &&
            (l1.Fr_port == l2.Fr_port) &&
            (l1.To_port == l2.To_port)
         )
      {
         return true;
      }
      return false;
   }
   
   enum
   {
       DEL_LINK = 3000,
       DEL_LINK_CON,
       SHOW_LINK_CONT,
       ADD_LINK_CON,
       SET_ACTIVE_LINK,
       //Aspen
       LINK_MENU,
       SHOW_LINK_NAME,
       LINK_INPUTS,
       LINK_OUTPUTS
   };
   
   wxPoint* GetPoint( size_t i );
   size_t GetNumberOfPoints( void );
   std::vector< wxPoint >* GetPoints( void );
   void SetPoint( wxPoint* pnt );
   unsigned int GetFromPort( void );
   unsigned int GetToPort( void );
   unsigned long GetFromModule( void );
   unsigned long GetToModule( void );

   void SetFromPort( unsigned int );
   void SetToPort( unsigned int );
   void SetFromModule( unsigned long );
   void SetToModule( unsigned long );
   Polygon* GetPolygon( void );

   void SetName(wxString name);
   wxString GetName();

   ///Helper functions
   void DrawLinkCon( bool flag, std::pair< double, double > scale, wxDC &dc );
   void CalcLinkPoly( void );
   void DrawLink( bool flag, wxDC& dc, std::pair< double, double > scale );
   double computenorm( wxPoint pt1, wxPoint pt2 );
   ///Set the user scale to enable working with the dc
   void SetDCScale( std::pair< double, double >* scale );

protected:
    void OnShowLinkContent( wxCommandEvent& event );
    void OnShowAspenName( wxCommandEvent& event );
    void OnQueryStreamInputs( wxCommandEvent &event );
    void OnQueryStreamOutputs( wxCommandEvent &event );
    void OnAddLinkCon( wxCommandEvent &event );
    void OnDelLink( wxCommandEvent &event );
    void OnDelLinkCon( wxCommandEvent &event );
    void OnMRightDown( wxMouseEvent &event );
    void OnSetActiveLinkID( wxUpdateUIEvent& event );
    bool SelectLink(int x, int y);
    ///Check the active id against the plugin id
    bool CheckID();

private:
    ///Name of the link, should be unique
    wxString linkName;
    ///The active link for the network
    wxString activeName;
    ///From plugin for the link
    unsigned long Fr_mod;
    ///To plugin for the link
    unsigned long To_mod;
    ///From port for the link
    unsigned int Fr_port;
    ///to port for the link
    unsigned int To_port;
    ///Used for the construction of arrow heads
    double sinb; 
    ///Used for the construction of arrow heads
    double cosb;
    ///Used for the construction of arrow heads
    double sina; 
    ///Used for the construction of arrow heads
    double cosa;    
    
    int m_selFrPort; // selected From port
    int m_selToPort; // selected To port;
    int m_selLinkCon; //selected Link Connector
    
    std::vector< wxPoint > cons; //connectors
    Polygon poly; //Poly is the current poly on the canvas
    wxScrolledWindow* networkFrame;
    //The mouse position when the right button 
    //clicked, used by menu event handlers
    wxPoint action_point;
    ///User scale
    /// first = x scale
    /// second = y scale
    std::pair< double, double >* userScale;
    
    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }
    
    DECLARE_EVENT_TABLE()
};
}
}

#define UILINK_CHECKID(event)  \
    if( !CheckID() ) \
    { \
        event.Skip(); \
        return; \
    } 

#endif
