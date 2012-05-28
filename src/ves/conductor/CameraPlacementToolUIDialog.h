/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

// --- VES Includes --- //
#include <ves/conductor/UIDialog.h>

#include <ves/conductor/util/DualSlider.h>

#include <ves/open/xml/DataValuePairPtr.h>

// --- wxWidgets Includes --- //
#include <wx/filepicker.h>
#include <wx/timer.h>

class wxRadioBox;
class wxSlider;
class wxSpinCtrl;
class wxButton;
class wxComboBox;
class wxBitmapButton;
class wxToggleButton;
class wxChoice;

// --- STL Includes --- //
#include <map>
#include <vector>
#include <string>

namespace ves
{
namespace conductor
{

namespace util
{
class CORBAServiceList;
class wxSpinCtrlDbl;
}

class VE_GUIPLUGINS_EXPORTS CameraPlacementToolUIDialog
    :
public ves::conductor::UIDialog
{
public:
    CameraPlacementToolUIDialog();
    CameraPlacementToolUIDialog(
        wxWindow* parent,
        int id,
        ves::conductor::util::CORBAServiceList* service );

    virtual ~CameraPlacementToolUIDialog();

    virtual bool TransferDataFromWindow();
    virtual bool TransferDataToWindow();
    virtual void Lock( bool l );

protected:

private:
    void BuildGUI();

    void SendCommandsToXplorer();
    void ClearInstructions();

    void OnAddCameraButton(
        wxCommandEvent& WXUNUSED( wxCommandEvent& event ) );
    void OnPrevCameraButton(
        wxCommandEvent& WXUNUSED( wxCommandEvent& event ) );
    void OnCameraComboBox( wxCommandEvent& event );
    void OnCameraComboBoxTextEnter( wxCommandEvent& event );
    void OnNextCameraButton(
        wxCommandEvent& WXUNUSED( wxCommandEvent& event ) );
    void OnDeleteCameraButton(
        wxCommandEvent& WXUNUSED( wxCommandEvent& event ) );
    void OnRemoveAllCamerasButton(
        wxCommandEvent& WXUNUSED( wxCommandEvent& event ) );

    void OnSaveImageButton(
        wxCommandEvent& WXUNUSED( wxCommandEvent& event ) );
    void OnSaveAllImagesButton(
        wxCommandEvent& WXUNUSED( wxCommandEvent& event ) );
    void OnImageDirPickerCtrl( wxFileDirPickerEvent& event );

    void OnToggleHighlightToolButton(
        wxCommandEvent& WXUNUSED( wxCommandEvent& event ) );
    void OnPrevMarkerButton(
        wxCommandEvent& WXUNUSED( wxCommandEvent& event ) );
    void OnMarkerComboBox( wxCommandEvent& event );
    void OnMarkerComboBoxTextEnter( wxCommandEvent& event );
    void OnNextMarkerButton(
        wxCommandEvent& WXUNUSED( wxCommandEvent& event ) );
    void OnDeleteMarkerButton(
        wxCommandEvent& WXUNUSED( wxCommandEvent& event ) );
    void OnRemoveAllMarkersButton(
        wxCommandEvent& WXUNUSED( wxCommandEvent& event ) );

    void OnDepthOfFieldEffectOnOffRadioBox( wxCommandEvent& event );
    void OnProjectionEffectOnOffRadioBox( wxCommandEvent& event );
    void OnProjectionEffectOpacitySlider( wxCommandEvent& WXUNUSED( event ) );

    void OnCameraWindowOnOffRadioBox( wxCommandEvent& event );
    void OnCameraWindowResolutionSlider( wxCommandEvent& WXUNUSED( event ) );

    void OnDepthHelperWindowOnOffRadioBox( wxCommandEvent& event );
    void OnDepthHelperWindowResolutionSlider( wxCommandEvent& event );

    void OnCameraGeometryOnOffRadioBox( wxCommandEvent& event );
    void OnFrustumGeometryOnOffRadioBox( wxCommandEvent& event );

    void OnFieldOfViewSpinCtrl( wxScrollEvent& WXUNUSED( event ) );
    void OnFieldOfViewText( wxCommandEvent& event );
    void OnFieldOfViewSlider( wxCommandEvent& WXUNUSED( event ) );
    void OnAspectRatioSpinCtrl( wxScrollEvent& WXUNUSED( event ) );
    void OnAspectRatioText( wxCommandEvent& event );
    void OnAspectRatioSlider( wxCommandEvent& WXUNUSED( event ) );
    void OnAspectRatioChoice( wxCommandEvent& event );
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

    void OnAutoComputerFarPlane( wxCommandEvent& event );
    void OnCameraManagerEvent( wxCommandEvent& event );
    void OnPictureModeEvent( wxCommandEvent& event );

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

    ///Idle function
    void OnTimer( wxTimerEvent& event );

    ///Get data back from xplorer
    void UpdateFromXplorerData();

    ///Get data back from xplorer
    void UpdateCameraData();

    ///Get data back from xplorer
    void UpdateMarkerData();

    int m_currentCameraSelection;
    unsigned int m_cameraNameNum;

    int m_currentMarkerSelection;
    unsigned int m_markerNameNum;

    double mProjectionData[ 4 ];
    double mDepthOfFieldData[ 3 ];

    wxButton* m_addCameraButton;
    wxBitmapButton* m_prevCameraButton;
    wxComboBox* m_cameraComboBox;
    wxBitmapButton* m_nextCameraButton;
    wxButton* m_deleteCameraButton;
    wxButton* m_removeAllCamerasButton;

    wxButton* m_saveImageButton;
    wxButton* m_saveAllImagesButton;
    wxDirPickerCtrl* m_imageDirPickerCtrl;

    wxRadioBox* m_cameraManagerButton;
    wxRadioBox* m_pictureModeButton;

    wxToggleButton* m_toggleHighlightToolButton;
    wxBitmapButton* m_prevMarkerButton;
    wxComboBox* m_markerComboBox;
    wxBitmapButton* m_nextMarkerButton;
    wxButton* m_deleteMarkerButton;
    wxButton* m_removeAllMarkersButton;

    wxRadioBox* mDepthOfFieldEffectOnOff;
    wxRadioBox* mProjectionEffectOnOff;
    wxSlider* mProjectionEffectOpacity;

    wxRadioBox* mCameraWindowOnOff;
    wxSlider* mCameraWindowResolution;
    wxRadioBox* mDepthHelperWindowOnOff;
    wxSlider* mDepthHelperWindowResolution;

    wxRadioBox* mCameraGeometryOnOff;
    wxRadioBox* mFrustumGeometryOnOff;

    ves::conductor::util::wxSpinCtrlDbl* mFieldOfViewSpinCtrl;
    wxSlider* mFieldOfViewSlider;
    ves::conductor::util::wxSpinCtrlDbl* mAspectRatioSpinCtrl;
    wxSlider* mAspectRatioSlider;

    wxRadioBox* m_autoComputeFarButton;

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

    wxChoice* m_aspectRatioChoice;

    wxTimer m_timer;

    ves::conductor::util::CORBAServiceList* mServiceList;
    std::string mCommandName;
    std::vector< ves::open::xml::DataValuePairSharedPtr > mInstructions;

    std::map< std::string, std::pair< unsigned int, unsigned int > > m_resolutionMap;
    DECLARE_EVENT_TABLE()
};
}
}

#endif //CAMERA_PLACEMENT_TOOL_UI_DIALOG_H
