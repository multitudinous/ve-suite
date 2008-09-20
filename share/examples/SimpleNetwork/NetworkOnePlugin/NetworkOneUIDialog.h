#ifndef NETWORK_ONE_PLUGIN_H
#define NETWORK_ONE_PLUGIN_H

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

class NetworkOneUIDialog : public ves::conductor::UIDialog
{
public:
	NetworkOneUIDialog( wxWindow* parent, int id, 
		ves::conductor::util::CORBAServiceList* service,
		std::string* mTextOne);
	virtual ~NetworkOneUIDialog();

	std::string* p_mTextOne;

	enum NETWORK_ONE_IDS
	{
		ID_TEXTCTRL
	};

	void BuildPage();

	void SetText( wxCommandEvent& event );

	wxTextCtrl* mTextOneCtrl;
	wxButton* mUpdateButton;

	ves::conductor::util::CORBAServiceList* mServiceList;

	std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }

	DECLARE_EVENT_TABLE();
};

#endif //NETWORK_ONE_PLUGIN_H