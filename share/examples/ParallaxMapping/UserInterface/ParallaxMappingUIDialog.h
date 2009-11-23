
#ifndef PARALLAX_MAPPING_UI_DIALOG_H
#define PARALLAX_MAPPING_UI_DIALOG_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIDialog.h>

//#include <ves/conductor/util/DualSlider.h>

#include <ves/open/xml/DataValuePairPtr.h>

namespace ves
{
namespace conductor
{
namespace util
{
class CORBAServiceList;
//class wxSpinCtrlDbl;
}
}
}

// --- STL Includes --- //
#include <vector>
#include <string>

class ParallaxMappingUIDialog : public ves::conductor::UIDialog
{
public:
    ParallaxMappingUIDialog();

    ParallaxMappingUIDialog(
        wxWindow* parent,
        int id,
        ves::conductor::util::CORBAServiceList* service );

    virtual ~ParallaxMappingUIDialog();

    virtual bool TransferDataFromWindow();
    virtual bool TransferDataToWindow();
    virtual void Lock( bool l );

protected:

private:
    void BuildGUI();

    void SendCommandsToXplorer();
    void ClearInstructions();

    ves::conductor::util::CORBAServiceList* mServiceList;
    std::string mCommandName;
    std::vector< ves::open::xml::DataValuePairSharedPtr > mInstructions;
    
    DECLARE_EVENT_TABLE()
};

#endif //PARALLAX_MAPPING_UI_DIALOG_H
