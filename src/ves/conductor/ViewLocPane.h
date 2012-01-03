/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#ifndef _VE_UI_VIEWLOC_H
#define _VE_UI_VIEWLOC_H
/*!\file ViewLocPane.h
  *ViewPoints/Flythrough Control Interface
  */
/*!\class VE_Conductor::
 * This class builds the user interface panel which contains all of
 * the controls for the view points and flythrough functionality
 */
#include <wx/gdicmn.h>
#include <ves/conductor/util/spinctld.h>
//#include <wx/spinctrl.h>
#include <wx/image.h>
#include <wx/dialog.h>
#include <wx/bmpbuttn.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/statbmp.h>
#include <wx/scrolwin.h>

#include <wx/arrstr.h>
#include <wx/utils.h>
#include <wx/button.h>
#include <wx/combobox.h>
#include <wx/listbox.h>
#include <wx/textctrl.h>
#include <wx/slider.h>
#include <wx/string.h>
#include <wx/notebook.h>
#include <wx/sizer.h>
#include <wx/app.h>
#include <wx/filename.h>
#include <wx/choicdlg.h>

#include <wx/filedlg.h>
#include <wx/textdlg.h>
#include <wx/msgdlg.h>

#include <vector>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/VEConfig.h>

namespace ves
{
namespace conductor
{
namespace util
{
class wxSpinCtrlDbl;
}
}
}

class VE_GUIPLUGINS_EXPORTS ViewLocPane : public wxDialog
{
public:
    ViewLocPane( wxWindow* parent );
    virtual ~ViewLocPane( void );

    void SendCommandsToXplorer();

    unsigned int _numStoredLocations;
    unsigned int _numStoredFlythroughs;
    unsigned int _vwptsInActiveFly;
    unsigned int _numViewLocLocal;
    unsigned int _vwptsInActiveFlyLocal;
    unsigned int _numStoredFlythroughsLocal;
    std::vector< std::vector <int> > flyThroughList;
    wxArrayString _locationName;
    wxArrayString _flythroughName;
    wxArrayString _activeFlyNames;
    wxArrayString _locNamesLocal;
    wxArrayString _activeFlyNamesLocal;
    wxArrayString _flythroughNamesLocal;

    short num_viewlocs;

protected:
    void _buildPage( void );

    void _rebuildNameArrays( void );
    void _setUpActiveFlyThroughNames( int );
    void _rebuildPage( void );
    void _resetSelections( void );

    std::string _commandName;///<The name of the command.
    std::vector<ves::open::xml::DataValuePairPtr> _dataValuePairList;///<The list of DataValuePairs
    int _numView_LocsGlobal;
    std::vector< ves::open::xml::CommandPtr > commands;
    int cId, cIso_value, cSc, cMin;
    std::string dataValueName;

    std::vector< double > commandInputs;
    wxScrolledWindow* scrollWindow;

    wxComboBox* _activeflySel;
    wxComboBox* _movetovwptSel;
    wxComboBox* _removevwptSel;
    wxComboBox* _addvptoflySel;
    wxListBox* _flybuilderListBox;
    wxComboBox* _insertvpinflySel;
    wxComboBox* _removevpfromflySel;
    wxComboBox* _deleteflySel;
    wxSlider* _speedCtrlSlider;
    ves::conductor::util::wxSpinCtrlDbl* _spinSpeedControls;

    //the controls
    void _onLoad( wxCommandEvent& event );
    void _onAcceptNewVPName( wxCommandEvent& event );
    void _onCancelNewVPName( wxCommandEvent& event );
    void _onRemoveVP( wxCommandEvent& event );
    void _onMoveToVP( wxCommandEvent& event );
    void _onBuildNewFlyButton( wxCommandEvent& event );
    void _onAcceptNewFlyName( wxCommandEvent& event );
    void _onCancelNewFlyName( wxCommandEvent& event );
    void _onActiveFlySel( wxCommandEvent& event );
    void _onAddVPtoFlySel( wxCommandEvent& event );
    void _onInsertVPinFlySel( wxCommandEvent& event );
    void _onRemoveVPfromFlySel( wxCommandEvent& event );
    void _onStartActiveFly( wxCommandEvent& event );
    void _onStopFly( wxCommandEvent& event );
    void _onFlyBuilderListBox( wxCommandEvent& event );
    void _onDeleteFlySel( wxCommandEvent& event );
    void _onSpeedChange( wxSpinEvent& event );

    ///Refresh the GUI
    ///\param event The idle event
    void _refreshGUIFromXplorerData( wxIdleEvent& event );

    ///Load a stored view points file
    void _onLoadStoredPointsFile( wxCommandEvent& event );

    ///Save current view points to a file.
    void _onSaveStoredPointsFile( wxCommandEvent& event );

    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }

    DECLARE_EVENT_TABLE()
};
#endif// _VE_UI_VIEWLOC_H
