#ifndef CAMERA_PLACEMENT_TOOL_UI_DIALOG_H
#define CAMERA_PLACEMENT_TOOL_UI_DIALOG_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIDialog.h>

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
    CameraPlacementToolUIDialog( wxWindow* parent,
                                 int id,
                                 ves::conductor::util::CORBAServiceList* service );

    enum CPT_IDS
    {
        ID_CAMERA_RADIOBOX,
        ID_FRUSTUM_RADIOBOX,
        ID_PROJECTION_RADIOBOX
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

    wxRadioBox* mCameraRadioBox;
    wxRadioBox* mFrustumRadioBox;
    wxRadioBox* mProjectionRadioBox;
    wxSlider* mFoVZSlider;
    wxSpinCtrl* mAspectRatioSpinCtrl;

    ves::conductor::util::CORBAServiceList* mServiceList;
    std::vector< ves::open::xml::DataValuePairSharedPtr > mInstructions;
    std::string mCommandName;

    DECLARE_EVENT_TABLE()

};
/*----------------------------------------------------------------------------*/

} //end cpt

#endif //CAMERA_PLACEMENT_TOOL_UI_DIALOG_H
