/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
#ifndef _DATASETLOADERUI_H_
#define _DATASETLOADERUI_H_
/*!\file DataSetLoaderUI.h
DataSetLoaderUI API
*/
/*!\class DataSetLoaderUI
* 
*/
#include <wx/string.h>
#include <wx/gdicmn.h>
#include <wx/dialog.h>
#include <wx/intl.h>

#include <set>
////@end includes

/*!
 * Forward declarations
 */

////@begin forward declarations
class wxComboBox;
class wxTextCtrl;
class wxButton;
class wxListBox;

namespace VE_XML
{
   class ParameterBlock;
   namespace VE_Model
   {
      class Model;
   }
}
////@end forward declarations

/*!
 * Control identifiers
 */

////@begin control identifiers
#define ID_DIALOG 10000
#define SYMBOL_DATASETLOADERUI_STYLE wxDEFAULT_DIALOG_STYLE|wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX|wxMAXIMIZE_BOX|wxMINIMIZE_BOX
#define SYMBOL_DATASETLOADERUI_TITLE _("DataSetLoader")
#define SYMBOL_DATASETLOADERUI_IDNAME ID_DIALOG
#define SYMBOL_DATASETLOADERUI_SIZE wxSize(-1, 500)
#define SYMBOL_DATASETLOADERUI_POSITION wxDefaultPosition

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

class DataSetLoaderUI: public wxDialog
{    
    //DECLARE_DYNAMIC_CLASS( DataSetLoaderUI )
    DECLARE_EVENT_TABLE()

public:
    /// Constructors
    DataSetLoaderUI( );
    DataSetLoaderUI( wxWindow* parent, 
                     wxWindowID id = SYMBOL_DATASETLOADERUI_IDNAME, 
                     const wxString& caption = SYMBOL_DATASETLOADERUI_TITLE, 
                     const wxPoint& pos = SYMBOL_DATASETLOADERUI_POSITION, 
                     const wxSize& size = SYMBOL_DATASETLOADERUI_SIZE, 
                     long style = SYMBOL_DATASETLOADERUI_STYLE, 
                     VE_XML::VE_Model::Model* veModel = 0);

   enum
   {
      ID_SCROLLEDWINDOW=10001,
      ID_COMBOBOX,
      ID_TEXTCTRL,
      ID_BUTTON,
      ID_TEXTCTRL2,
      ID_BUTTON3,
      ID_TEXTCTRL3,
      ID_BUTTON4,
      ID_BUTTON6,
      ID_BUTTON5,
      ID_TEXTCTRL1,
      ID_BUTTON2,
      ID_SCROLLEDWINDOW1,
      ID_LISTBOX,
      ID_INFORMATION_PACKET_LIST,
      ID_INFORMATION_PACKET_CHANGE_NAME,
      ID_INFORMATION_PACKET_ADD_NAME,
      ID_ADD_DATASET,
      ID_DELETE_DATASET
   };

    /// Creation
    bool Create( wxWindow* parent, 
                  wxWindowID id = SYMBOL_DATASETLOADERUI_IDNAME, 
                  const wxString& caption = SYMBOL_DATASETLOADERUI_TITLE, 
                  const wxPoint& pos = SYMBOL_DATASETLOADERUI_POSITION, 
                  const wxSize& size = SYMBOL_DATASETLOADERUI_SIZE, 
                  long style = SYMBOL_DATASETLOADERUI_STYLE, 
                  VE_XML::VE_Model::Model* veModel = 0);

    /// Creates the controls and sizers
    void CreateControls();

////@begin DataSetLoaderUI event handler declarations

    /// wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON
    void OnButtonClick( wxCommandEvent& event );

    /// wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON3
    void OnButton3Click( wxCommandEvent& event );

    /// wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON4
    void OnButton4Click( wxCommandEvent& event );

    /// wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON6
    void OnButton6Click( wxCommandEvent& event );

    /// wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON2
    void OnButton2Click( wxCommandEvent& event );

    /// wxEVT_COMMAND_LISTBOX_SELECTED event handler for ID_LISTBOX
    void OnListboxSelected( wxCommandEvent& event );

    /// wxEVT_COMMAND_COMBOBOX_SELECTED event handler for ID_INFORMATION_PACKET_LIST
    void OnInformationPacketChange( wxCommandEvent& event );

    /// wxEVT_COMMAND_TEXT_ENTER event handler for ID_INFORMATION_PACKET_CHANGE_NAME
    void OnInformationPacketAdd( wxCommandEvent& event );

    /// wxEVT_COMMAND_TEXT_UPDATED event handler for ID_INFORMATION_PACKET_ADD_NAME
    void OnInformationPacketChangeName( wxCommandEvent& event );

   std::string GetActiveDataSetName();
   VE_XML::ParameterBlock* GetParamBlock();
   
   ///Disable/enable the buttons
   ///\param flag Bool to control the ui.
   void EnableUI( bool flag );
   ///Initialize widgets
   void InitializeWidgets( void );
   ///Set the text controls when the text is modified
   void SetTextCtrls( void );

   ///Check for dataset name in the combo box
   ///\param name The name of the dataset to search for.
   bool DatasetExists(std::string name);
   
////@end DataSetLoaderUI event handler declarations

////@begin DataSetLoaderUI member function declarations

    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );
////@end DataSetLoaderUI member function declarations

    /// Should we show tooltips?
    static bool ShowToolTips();

////@begin DataSetLoaderUI member variables
   wxComboBox* dataSetList;
   wxTextCtrl* dataSetTextEntry;
   wxButton* dataSetOpenButton;
   wxTextCtrl* preComputDirTextEntry;
   wxButton* preComputeOpenButton;
   wxTextCtrl* surfaceDataText;
   wxButton* surfaceDataOpenButton;
   wxButton* transformButton;
   wxButton* scalarButton;
   wxButton* itemButton22;
   //wxButton* createDataSet;
   wxListBox* itemListBox24;
   wxTextCtrl* itemTextCtrl21;  
   wxStaticBox* itemStaticBoxSizer6Static;
   wxStaticBox* itemStaticBoxSizer9Static;
   wxStaticBox* itemStaticBoxSizer12Static;
   wxStaticBox* itemStaticBoxSizer15Static;
   wxStaticBox* itemStaticBoxSizer19Static;
   std::set< wxString > textureDirs;

   wxArrayString _availableDatasets;///<The list of datasets(should this be called something else?)
////@end DataSetLoaderUI member variables
   VE_XML::VE_Model::Model* veModel;
   VE_XML::ParameterBlock* paramBlock;

   VE_XML::VE_Model::Model* GetModel( void );

   int lastAddition;
};

#endif
    // _DATASETLOADERUI_H_
