/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#ifndef INT_STOVES_UI_DIALOG_H
#define INT_STOVES_UI_DIALOG_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIDialog.h>

#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/CommandPtr.h>

namespace ves
{
namespace conductor
{
	class UIPluginBase;
namespace util
{
    class CORBAServiceList;
}
}
}

// --- wxWidgets Includes --- //
#include <wx/spinctrl.h>

class wxComboBox;
class wxButton;
class wxTextCtrl;
class wxStaticBox;
class wxStaticBoxSizer;
class wxBoxSizer;
class wxCheckBox;

class GLCanvasWrapper;

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

class IntStoves_UI_Dialog : public ves::conductor::UIDialog
{
public:
    IntStoves_UI_Dialog();
    IntStoves_UI_Dialog( wxWindow* parent,
                         int id,
                         long* numbaffles,
                         std::vector< double >* baffle1,
                         std::vector< double >* baffle2,
                         std::vector< double >* baffle3,
                         std::vector< double >* baffle4,
                         std::vector< double >* baffle5,
                         std::vector< double >* baffle6,
                         std::vector< double >* baffle7 );

    virtual ~IntStoves_UI_Dialog();

    enum INTERACTIVE_STOVES_IDS
    {
        NUMBAFFSEL_COMBOBOX,
        DESIGN_BUTTON,
        ACTBAFFSEL_COMBOBOX,
        ADDBAFF_BUTTON,
        REMOVEBAFF_BUTTON,
        REMOVEBAFF_COMBOBOX,
        CHANGE_DEPTH,
        UPDATE_PARAMS,
        VECTOR_CHECKBOX,
        CONTOUR_CHECKBOX
    };

    virtual bool TransferDataFromWindow();
    virtual bool TransferDataToWindow();
    virtual void Lock( bool l );

protected:

private:

public:
    void _buildPage();
    //void _onNumBafSel(wxCommandEvent& event);
    void _onActBafSel(wxCommandEvent& event);
    //void _onDesignStove(wxCommandEvent& event);
    void _onAddBaff();
    void _onRemoveBaff(wxCommandEvent& event);
    void SetDepth(wxSpinEvent& event);
    void _reDrawBaff(int);
    void _removeBaff(int);
    void _rebuildActBaffSel();
    void _reOrganizeBaffs();
    void SetBaffleData();
    void UpdateParams(wxCommandEvent& event);
    void ShowVectors(wxCommandEvent& event);
    void ShowContour(wxCommandEvent& event);

    long* p_numbaffles;
    std::vector< double >* p_baffle1;
    std::vector< double >* p_baffle2;
    std::vector< double >* p_baffle3;
    std::vector< double >* p_baffle4;
    std::vector< double >* p_baffle5;
    std::vector< double >* p_baffle6;
    std::vector< double >* p_baffle7;
    //GUI Variables
    wxString baffnums[7];
    wxString activebaff[7];
    bool actbaffdrawn[7];
    std::vector<double>* temp[7];
    int m_numbaffles;

    int GetStartX(int index);
    int GetStartY(int index);
    int GetDirection(int index);
    int GetLength(int index);
    int GetDepth(int index);
    int GetNumBaffles();

    std::vector< double > baffleParams;

    std::vector< ves::open::xml::DataValuePair > parameters;

    ves::open::xml::CommandPtr m_command;

    void SendCommandsToXplorer();
    void ClearParameters();
    std::string command_name;
    ves::conductor::util::CORBAServiceList* serviceList;

    wxComboBox* _numbaffsel;
    wxComboBox* _activebaffsel;
    wxComboBox* _removebafCombo;
    wxButton* _addbafButton;
    wxButton* _removebafButton;
    wxButton* _updateButton;
    wxButton* m_closeButton;
    wxButton* _designButton;
    GLCanvasWrapper* mCanvasWrapper;
    wxStaticBox* _baff[7];
    wxStaticBoxSizer* _baffGroup[7];
    wxTextCtrl* _startposx[7];
    wxTextCtrl* _startposy[7];
    wxTextCtrl* _direction[7];
    wxTextCtrl* _length[7];
    wxSpinCtrl* _depth[7];
    wxBoxSizer* _rightset;	
    wxBoxSizer* m_buttons;
    wxBoxSizer* m_checkBoxes;
    wxCheckBox* m_vectorCheckBox;
    wxCheckBox* m_contourCheckBox;

    unsigned int vectors;
    unsigned int contour;

    DECLARE_EVENT_TABLE();
};

#endif //INT_STOVES_UI_DIALOG_H
