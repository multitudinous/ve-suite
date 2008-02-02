#ifndef COIN_FUNNEL_UI_DIALOG_H
#define COIN_FUNNEL_UI_DIALOG_H

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

class CoinFunnelUIDialog : public ves::conductor::UIDialog
{
public:
    CoinFunnelUIDialog( wxWindow* parent, int id, ves::conductor::util::CORBAServiceList* service, std::string* portNumber );
    CoinFunnelUIDialog(){;}

    enum
    {
        OK_BUTTON
    };

    virtual ~CoinFunnelUIDialog();

    virtual bool TransferDataFromWindow();
    virtual bool TransferDataToWindow();
    virtual void Lock( bool l );

    std::string* p_portNumber;

protected:
    void BuildGUI();
    void UpdateGUI();

    void OnOK( wxCommandEvent& event );

    std::vector< ves::open::xml::DataValuePairSharedPtr > instructions;///<The DataValuePairs for the current command
    std::string command_name;///<The command name

    void SendCommandsToXplorer();
    void ClearInstructions();

    ves::conductor::util::CORBAServiceList* serviceList;
    DECLARE_EVENT_TABLE()

};

#endif //COIN_FUNNEL_UI_DIALOG_H
