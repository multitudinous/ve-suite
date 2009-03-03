#ifndef NETWORK_THREE_PLUGIN_H
#define NETWORK_THREE_PLUGIN_H

#include <ves/conductor/UIDialog.h>

class wxTextCtrl;
class wxButton;

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

class NetworkThreeUIDialog : public ves::conductor::UIDialog
{
public:
	NetworkThreeUIDialog( wxWindow* parent, int id, 
		ves::conductor::util::CORBAServiceList* service,
        std::vector< std::string >* mNetworkThreeInputs);
	virtual ~NetworkThreeUIDialog();

    std::vector< std::string >* p_mNetworkThreeInputs;

	enum NETWORK_THREE_IDS
	{
		INPUT_ONE_TEXTCTRL,
        INPUT_TWO_TEXTCTRL,
        INPUT_THREE_TEXTCTRL
	};

	void BuildPage();

	void SetText( wxCommandEvent& event );

    wxTextCtrl* mTextCtrlOne;
    wxTextCtrl* mTextCtrlTwo;
	wxTextCtrl* mTextCtrlThree;
	wxButton* mUpdateButton;

	ves::conductor::util::CORBAServiceList* mServiceList;

	std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }

	DECLARE_EVENT_TABLE();
};

#endif //NETWORK_THREE_PLUGIN_H