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
/*----------------------------------------------------------------------------*/
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
        ID_ASPECTRATIO_SPINCTRL
    };

    virtual ~CameraPlacementToolUIDialog();

    virtual bool TransferDataFromWindow();
    virtual bool TransferDataToWindow();
    virtual void Lock( bool l );

    void SetCommandName( const std::string& commandName );
    void AddInstruction( ves::open::xml::DataValuePairSharedPtr instruction );

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

    class NearPlaneSliderCallback :
        public ves::conductor::util::DualSlider::SliderCallback
    {
    public:
        NearPlaneSliderCallback( CameraPlacementToolUIDialog* dialog );
        virtual ~NearPlaneSliderCallback();

        virtual void SliderOperation();
        
    private:
        CameraPlacementToolUIDialog* mDialog;
    };

    class NearFarPlaneSliderCallback :
        public ves::conductor::util::DualSlider::SliderCallback
    {
    public:
        NearFarPlaneSliderCallback( CameraPlacementToolUIDialog* dialog );
        virtual ~NearFarPlaneSliderCallback();

        virtual void SliderOperation();
        
    private:
        CameraPlacementToolUIDialog* mDialog;
    };

    class FarPlaneSliderCallback :
        public ves::conductor::util::DualSlider::SliderCallback
    {
    public:
        FarPlaneSliderCallback( CameraPlacementToolUIDialog* dialog );
        virtual ~FarPlaneSliderCallback();

        virtual void SliderOperation();

    private:
        CameraPlacementToolUIDialog* mDialog;
    };

    wxRadioBox* mCameraRadioBox;
    wxRadioBox* mFrustumRadioBox;
    wxRadioBox* mProjectionRadioBox;
    wxSlider* mFoVZSlider;
    ves::conductor::util::wxSpinCtrlDbl* mAspectRatioSpinCtrl;
    ves::conductor::util::DualSlider* mNearFarPlaneDualSlider;

    ves::conductor::util::CORBAServiceList* mServiceList;
    std::string mCommandName;
    std::vector< ves::open::xml::DataValuePairSharedPtr > mInstructions;
    
    DECLARE_EVENT_TABLE()

};
/*----------------------------------------------------------------------------*/

} //end cpt

#endif //CAMERA_PLACEMENT_TOOL_UI_DIALOG_H
