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
        ID_DRUM_ANIMATION_ON_OFF,
        ID_CAMERA_GEOMETRY_ON_OFF,
        ID_FRUSTUM_GEOMETRY_ON_OFF,

        ID_DEPTH_OF_FIELD_EFFECT_ON_OFF,
        ID_PROJECTION_EFFECT_ON_OFF,
        ID_PROJECTION_EFFECT_OPACITY,

        ID_CAMERA_WINDOW_ON_OFF,
        ID_CAMERA_WINDOW_RESOLUTION,

        ID_DEPTH_HELPER_WINDOW_ON_OFF,
        ID_DEPTH_HELPER_WINDOW_RESOLUTION,

        ID_FIELD_OF_VIEW_SPINCTRL,
        ID_FIELD_OF_VIEW_SLIDER,
        ID_ASPECT_RATIO_SPINCTRL,
        ID_ASPECT_RATIO_SLIDER,
		ID_NEAR_PLANE_SPINCTRL,
		ID_NEAR_PLANE_SLIDER,
		ID_FAR_PLANE_SPINCTRL,
		ID_FAR_PLANE_SLIDER,

        ID_FOCAL_DISTANCE_SPINCTRL,
        ID_FOCAL_DISTANCE_SLIDER,
		ID_FOCAL_RANGE_SPINCTRL,
		ID_FOCAL_RANGE_SLIDER,
		ID_MAX_CIRCLE_OF_CONFUSION_SPINCTRL,
		ID_MAX_CIRCLE_OF_CONFUSION_SLIDER,
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

    void OnDrumAnimationOnOffRadioBox( wxCommandEvent& event );
    void OnCameraGeometryOnOffRadioBox( wxCommandEvent& event );
    void OnFrustumGeometryOnOffRadioBox( wxCommandEvent& event );

    void OnDepthOfFieldEffectOnOffRadioBox( wxCommandEvent& event );
    void OnProjectionEffectOnOffRadioBox( wxCommandEvent& event );
    void OnProjectionEffectOpacitySlider( wxCommandEvent& WXUNUSED( event ) );

    void OnCameraWindowOnOffRadioBox( wxCommandEvent& event );
    void OnCameraWindowResolutionSlider( wxCommandEvent& WXUNUSED( event ) );

    void OnDepthHelperWindowOnOffRadioBox( wxCommandEvent& event );
    void OnDepthHelperWindowResolutionSlider( wxCommandEvent& event );

    void OnFieldOfViewSpinCtrl( wxScrollEvent& WXUNUSED( event ) );
	void OnFieldOfViewText( wxCommandEvent& event );
    void OnFieldOfViewSlider( wxCommandEvent& WXUNUSED( event ) );
    void OnAspectRatioSpinCtrl( wxScrollEvent& WXUNUSED( event ) );
    void OnAspectRatioText( wxCommandEvent& event );
    void OnAspectRatioSlider( wxCommandEvent& WXUNUSED( event ) );
	void OnNearPlaneSpinCtrl( wxScrollEvent& WXUNUSED( event ) );
	void OnNearPlaneText( wxCommandEvent& event );
	void OnNearPlaneSlider( wxCommandEvent& WXUNUSED( event ) );
	void OnFarPlaneSpinCtrl( wxScrollEvent& WXUNUSED( event ) );
	void OnFarPlaneText( wxCommandEvent& event );
	void OnFarPlaneSlider( wxCommandEvent& WXUNUSED( event ) );

    void OnFocalDistanceSpinCtrl( wxScrollEvent& WXUNUSED( event ) );
    void OnFocalDistanceText( wxCommandEvent& event );
    void OnFocalDistanceSlider( wxCommandEvent& WXUNUSED( event ) );
	void OnFocalRangeSpinCtrl( wxScrollEvent& WXUNUSED( event ) );
	void OnFocalRangeText( wxCommandEvent& event );
	void OnFocalRangeSlider( wxCommandEvent& WXUNUSED( event ) );
	void OnMaxCircleOfConfusionSpinCtrl( wxScrollEvent& WXUNUSED( event ) );
	void OnMaxCircleOfConfusionText( wxCommandEvent& event );
	void OnMaxCircleOfConfusionSlider( wxCommandEvent& WXUNUSED( event ) );

    bool EnsureSliders( int activeSliderID );

    void UpdateFieldOfViewControls();
    void UpdateAspectRatioControls();
	void UpdateNearPlaneControls();
	void UpdateFarPlaneControls();

    void UpdateFocalDistanceControls();
    void UpdateFocalRangeControls();
    void UpdateMaxCircleOfConfusionControls();

    void ProjectionUpdate();
    void FocalDistanceUpdate();
    void FocalRangeUpdate();
    void MaxCircleOfConfusionUpdate();

    double mProjectionData[ 4 ];
    double mDepthOfFieldData[ 3 ];

    wxRadioBox* mDrumAnimationOnOff;
    wxRadioBox* mCameraGeometryOnOff;
    wxRadioBox* mFrustumGeometryOnOff;

    wxRadioBox* mDepthOfFieldEffectOnOff;
    wxRadioBox* mProjectionEffectOnOff;
    wxSlider* mProjectionEffectOpacity;

    wxRadioBox* mCameraWindowOnOff;
    wxSlider* mCameraWindowResolution;
    wxRadioBox* mDepthHelperWindowOnOff;
    wxSlider* mDepthHelperWindowResolution;

    ves::conductor::util::wxSpinCtrlDbl* mFieldOfViewSpinCtrl;
    wxSlider* mFieldOfViewSlider;
    ves::conductor::util::wxSpinCtrlDbl* mAspectRatioSpinCtrl;
    wxSlider* mAspectRatioSlider;
    ves::conductor::util::wxSpinCtrlDbl* mNearPlaneSpinCtrl;
    wxSlider* mNearPlaneSlider;
    ves::conductor::util::wxSpinCtrlDbl* mFarPlaneSpinCtrl;
    wxSlider* mFarPlaneSlider;

    ves::conductor::util::wxSpinCtrlDbl* mFocalDistanceSpinCtrl;
    wxSlider* mFocalDistanceSlider;
    ves::conductor::util::wxSpinCtrlDbl* mFocalRangeSpinCtrl;
    wxSlider* mFocalRangeSlider;
    ves::conductor::util::wxSpinCtrlDbl* mMaxCircleOfConfusionSpinCtrl;
    wxSlider* mMaxCircleOfConfusionSlider;

    ves::conductor::util::CORBAServiceList* mServiceList;
    std::string mCommandName;
    std::vector< ves::open::xml::DataValuePairSharedPtr > mInstructions;
    
    DECLARE_EVENT_TABLE()
};

} //end cpt

#endif //CAMERA_PLACEMENT_TOOL_UI_DIALOG_H
