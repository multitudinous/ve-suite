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

#ifndef CAMERA_PLACEMENT_TOOL_UI_DIALOG_H
#define CAMERA_PLACEMENT_TOOL_UI_DIALOG_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIDialog.h>

#include <ves/conductor/util/DualSlider.h>

namespace ves
{
namespace conductor
{
namespace util
{
class CORBAServiceList;
class wxSpinCtrlDbl;
}
}
}

// --- wxWidgets Includes --- //
class wxRadioBox;
class wxSlider;
class wxSpinCtrl;

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

namespace cpt
{

class CameraPlacementToolUIDialog : public ves::conductor::UIDialog
{
public:
    CameraPlacementToolUIDialog();
    CameraPlacementToolUIDialog( 
        wxWindow* parent,
        int id,
        ves::conductor::util::CORBAServiceList* service );

    enum CPT_IDS
    {
        ID_CAMERA_RADIOBOX,
        ID_FRUSTUM_RADIOBOX,
        ID_PROJECTION_RADIOBOX,
        ID_FOVZ_SLIDER,
        ID_ASPECTRATIO_SPINCTRL,
		ID_NEARPLANE_SPINCTRL,
		ID_NEARPLANE_SLIDER,
		ID_FARPLANE_SPINCTRL,
		ID_FARPLANE_SLIDER,
        ID_CAMERAVIEW_RADIOBOX,
        ID_RESOLUTION_SLIDER
    };

    virtual ~CameraPlacementToolUIDialog();

    virtual bool TransferDataFromWindow();
    virtual bool TransferDataToWindow();
    virtual void Lock( bool l );

protected:

private:
    void BuildGUI();

    void SendCommandsToXplorer();
    void ClearInstructions();

    void OnCameraRadioBox( wxCommandEvent& event );
    void OnFrustumRadioBox( wxCommandEvent& event );
    void OnProjectionRadioBox( wxCommandEvent& event );
    void OnFoVZSlider( wxCommandEvent& WXUNUSED( event ) );
    void OnAspectRatioSpinCtrl( wxScrollEvent& WXUNUSED( event ) );
    void OnCameraViewRadioBox( wxCommandEvent& event );
    void OnResolutionSlider( wxCommandEvent& WXUNUSED( event ) );
	void OnNearPlaneSpinCtrl( wxScrollEvent& WXUNUSED( event ) );
	void OnNearPlaneText( wxCommandEvent& event );
	void OnNearPlaneSlider( wxCommandEvent& WXUNUSED( event ) );
	void OnFarPlaneSpinCtrl( wxScrollEvent& WXUNUSED( event ) );
	void OnFarPlaneText( wxCommandEvent& event );
	void OnFarPlaneSlider( wxCommandEvent& WXUNUSED( event ) );
	bool EnsureSliders( int activeSliderID );
	void UpdateNearPlaneControls();
	void UpdateFarPlaneControls();
    void ProjectionUpdate();

    double mProjectionData[ 4 ];

    wxRadioBox* mCameraRadioBox;
    wxRadioBox* mFrustumRadioBox;
    wxRadioBox* mProjectionRadioBox;
    wxSlider* mFoVZSlider;
    ves::conductor::util::wxSpinCtrlDbl* mAspectRatioSpinCtrl;
	ves::conductor::util::wxSpinCtrlDbl* mNearPlaneSpinCtrl;
	wxSlider* mNearPlaneSlider;
	ves::conductor::util::wxSpinCtrlDbl* mFarPlaneSpinCtrl;
	wxSlider* mFarPlaneSlider;
    wxRadioBox* mCameraViewRadioBox;
    wxSlider* mResolutionSlider;

    ves::conductor::util::CORBAServiceList* mServiceList;
    std::string mCommandName;
    std::vector< ves::open::xml::DataValuePairSharedPtr > mInstructions;
    
    DECLARE_EVENT_TABLE()

};

} //end cpt

#endif //CAMERA_PLACEMENT_TOOL_UI_DIALOG_H
