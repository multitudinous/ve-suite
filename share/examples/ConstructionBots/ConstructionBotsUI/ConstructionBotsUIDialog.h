#ifndef CONSTRUCTION_BOTS_UI_DIALOG_H
#define CONSTRUCTION_BOTS_UI_DIALOG_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIDialog.h>

#include <ves/open/xml/DataValuePairPtr.h>

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

class ConstructionBotsUIDialog : public ves::conductor::UIDialog
{
public:
    ConstructionBotsUIDialog();
    ConstructionBotsUIDialog( wxWindow* parent,
                              int id,
                              ves::conductor::util::CORBAServiceList* service );
    
    enum CONSTRUCTION_BOTS_IDS
    {
        
    };

    virtual ~ConstructionBotsUIDialog();

    virtual bool TransferDataFromWindow();
    virtual bool TransferDataToWindow();
    virtual void Lock( bool l );

protected:

private:
    void BuildGUI();
    void UpdateGUI();

    void SendCommandsToXplorer();
    void ClearInstructions();

    ves::conductor::util::CORBAServiceList* mServiceList;

    std::vector< ves::open::xml::DataValuePairSharedPtr > mInstructions;
    std::string mCommandName;

    DECLARE_EVENT_TABLE()
};

#endif //CONSTRUCTION_BOTS_UI_DIALOG_H
