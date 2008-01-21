/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef _DATASETLOADERUI_H_
#define _DATASETLOADERUI_H_
/*!\file DataSetLoaderUI.h
DataSetLoaderUI API
*/
/*!\class DataSetLoaderUI
*
*/
#include <ves/VEConfig.h>

#include <ves/open/xml/model/ModelPtr.h>
#include <ves/open/xml/DataValuePairPtr.h>

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
class wxStaticBox;

namespace ves
{
namespace open
{
namespace xml
{
class ParameterBlock;
}
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

namespace ves
{
namespace conductor
{
namespace util
{
class VE_CONDUCTOR_UTILS_EXPORTS DataSetLoaderUI: public wxDialog
{
    //DECLARE_DYNAMIC_CLASS( DataSetLoaderUI )

public:
    /// Constructors
    DataSetLoaderUI( );
    DataSetLoaderUI( wxWindow* parent,
                     wxWindowID id = SYMBOL_DATASETLOADERUI_IDNAME,
                     const wxString& caption = SYMBOL_DATASETLOADERUI_TITLE,
                     const wxPoint& pos = SYMBOL_DATASETLOADERUI_POSITION,
                     const wxSize& size = SYMBOL_DATASETLOADERUI_SIZE,
                     long style = SYMBOL_DATASETLOADERUI_STYLE,
                     ves::open::xml::model::ModelWeakPtr veModel = 0 );

    enum
    {
        ID_SCROLLEDWINDOW = 10001,
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
                 ves::open::xml::model::ModelWeakPtr veModel = 0 );

    /// Creates the controls and sizers
    void CreateControls();
    ///Send commands to xplorer as events are processed
    void SendCommandToXplorer( ves::open::xml::DataValuePairSharedPtr tempObject );

////@begin DataSetLoaderUI event handler declarations

    /// wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON
    void OnLoadFile( wxCommandEvent& event );

    /// wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON3
    void OnButton3Click( wxCommandEvent& event );

    /// wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON4
    void OnLoadSurfaceFile( wxCommandEvent& event );

    /// wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON6
    void OnTransformDataset( wxCommandEvent& event );

    /// wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON2
    void OnLoadTextureFile( wxCommandEvent& event );

    /// wxEVT_COMMAND_LISTBOX_SELECTED event handler for ID_LISTBOX
    void OnListboxSelected( wxCommandEvent& event );

    /// wxEVT_COMMAND_COMBOBOX_SELECTED event handler for ID_INFORMATION_PACKET_LIST
    void OnInformationPacketChange( wxCommandEvent& event );

    /// wxEVT_COMMAND_TEXT_ENTER event handler for ID_INFORMATION_PACKET_CHANGE_NAME
    void OnInformationPacketAdd( wxCommandEvent& event );

    /// wxEVT_COMMAND_TEXT_ENTER event handler for ID_INFORMATION_PACKET_CHANGE_NAME
    void OnDeleteDataset( wxCommandEvent& event );

    /// wxEVT_COMMAND_TEXT_UPDATED event handler for ID_INFORMATION_PACKET_ADD_NAME
    void OnInformationPacketChangeName( wxCommandEvent& event );

    std::string GetActiveDataSetName();
    ves::open::xml::ParameterBlock* GetParamBlock();

    ///Disable/enable the buttons
    ///\param flag Bool to control the ui.
    void EnableUI( bool flag );
    ///Initialize widgets
    void InitializeWidgets( void );
    ///Set the text controls when the text is modified
    void SetTextCtrls( void );

////@end DataSetLoaderUI event handler declarations

////@begin DataSetLoaderUI member function declarations

    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );
////@end DataSetLoaderUI member function declarations

    /// Should we show tooltips?
    static bool ShowToolTips();

private:
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

    ves::open::xml::model::ModelPtr m_veModel;
    ves::open::xml::ParameterBlock* paramBlock;

    int lastAddition;
    DECLARE_EVENT_TABLE()
};
}
}
}
#endif
// _DATASETLOADERUI_H_
