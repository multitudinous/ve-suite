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
        OK_BUTTON
    };

    virtual ~CameraPlacementToolUIDialog();

    virtual bool TransferDataFromWindow();
    virtual bool TransferDataToWindow();
    virtual void Lock( bool l );

protected:

private:
    void BuildGUI();
    void UpdateGUI();

    void SendCommandsToXplorer();
    void ClearInstructions();

    void OnOK( wxCommandEvent& event );

    ves::conductor::util::CORBAServiceList* mServiceList;
    std::vector< ves::open::xml::DataValuePairSharedPtr > mInstructions;
    std::string mCommandName;

    DECLARE_EVENT_TABLE()

};
/*----------------------------------------------------------------------------*/

} //end cpt

#endif //CAMERA_PLACEMENT_TOOL_UI_DIALOG_H
