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
		std::string* mTextThree);
	virtual ~NetworkThreeUIDialog();

	std::string* p_mTextThree;

	enum NETWORK_THREE_IDS
	{
		ID_TEXTCTRL
	};

	void BuildPage();

	void SetText( wxCommandEvent& event );

	wxTextCtrl* mTextThreeCtrl;
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