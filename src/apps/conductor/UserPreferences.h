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
#ifndef USERPREFERENCES_H_
#define USERPREFERENCES_H_
/*!\file UserPreferences.h
UserPreferences API
*/
/*!\class UserPreferences
* 
*/
#include <vector>

#include <wx/string.h>
#include <wx/gdicmn.h>
#include <wx/intl.h>
#include <wx/propdlg.h>

class wxCheckListBox;
class wxButton;
class wxCheckBox;
class wxColourData;

namespace ves
{
namespace conductor
{
namespace util
{
    class CORBAServiceList;
}
}
}

#include <map>
#include <string>

/*!
 * Control identifiers
 */

////@begin control identifiers
#define ID_PREFERENCES_DIALOG 20000
#define SYMBOL_USERPREFERENCES_STYLE wxDEFAULT_DIALOG_STYLE|wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX|wxMAXIMIZE_BOX|wxMINIMIZE_BOX
#define SYMBOL_USERPREFERENCES_TITLE _("UserPrefences")
#define SYMBOL_USERPREFERENCES_IDNAME ID_PREFERENCES_DIALOG
#define SYMBOL_USERPREFERENCES_SIZE wxSize(-1, 500)
#define SYMBOL_USERPREFERENCES_POSITION wxDefaultPosition

////@end control identifiers

/*!
 * Compatibility
 */

#ifndef wxCLOSE_BOX
#define wxCLOSE_BOX 0x1000
#endif

/*!
 * DataSetLoaderUI class declaration
 */

class UserPreferences: public wxPropertySheetDialog
{    
public:
   /// Constructors
   UserPreferences( );
   UserPreferences( wxWindow* parent, 
                  wxWindowID id = SYMBOL_USERPREFERENCES_IDNAME, 
                  const wxString& caption = SYMBOL_USERPREFERENCES_TITLE, 
                  const wxPoint& pos = SYMBOL_USERPREFERENCES_POSITION, 
                  const wxSize& size = SYMBOL_USERPREFERENCES_SIZE, 
                  long style = SYMBOL_USERPREFERENCES_STYLE);

   enum
   {
        ID_NAVIGATION_CHKBX,
	    ID_BACKGROUND_COLOR_BUTTON,
        ID_SHUTDOWN_XPLORER,
        ID_CONDUCTOR_CHKBX
   };

   virtual ~UserPreferences();
   
   /// Creation
   bool Create( wxWindow* parent, 
               wxWindowID id = SYMBOL_USERPREFERENCES_IDNAME, 
               const wxString& caption = SYMBOL_USERPREFERENCES_TITLE, 
               const wxPoint& pos = SYMBOL_USERPREFERENCES_POSITION, 
               const wxSize& size = SYMBOL_USERPREFERENCES_SIZE, 
               long style = SYMBOL_USERPREFERENCES_STYLE);

   /// Creates the controls and sizers
   void CreateControls();

    ///Check to set map for navigation pane
    void OnNavigationCheck( wxCommandEvent& event );
    ///Check to set map for background color
    void OnSetBackgroundColor( wxCommandEvent& event );
    ///Check to set map for Xplorer shutdown option
    void OnShutdownXplorer( wxCommandEvent& event );
    void OnConductorCheck( wxCommandEvent& event );
   bool GetMode( std::string mode );
   void ReadConfiguration( void );
   void WriteConfiguration( void );
      
   /// Should we show tooltips?
   static bool ShowToolTips();

   ///Returns the chosen background color
   std::vector< double > GetBackgroundColor( void );

    wxCheckBox* shutdownModeChkBx;///<Check box for Xplorer shutdown option
   
private:
   wxCheckListBox* prefChkBx;///<The check box list of preferences

   wxButton* backgroundColorButton;///<To choose background color

   wxColourData* xplorerWxColor;
   std::vector<double> xplorerColor;
   //wxColour* colour;

   ves::conductor::util::CORBAServiceList* serviceList;

   std::map< std::string, double > backgroundColor; ///Map the colors to their values

   std::map< std::string, bool > preferenceMap; ///<Map to hold preference bools and key names
   
   std::string ConvertUnicode( const wxChar* data )
   {
      std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
      return tempStr;
   }

   DECLARE_EVENT_TABLE()

};
#endif
