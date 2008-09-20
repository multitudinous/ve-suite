#ifndef NETWORK_TWO_PLUGIN_H
#define NETWORK_TWO_PLUGIN_H

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

class NetworkTwoUIDialog : public ves::conductor::UIDialog
{
public:
	NetworkTwoUIDialog( wxWindow* parent, int id, 
		ves::conductor::util::CORBAServiceList* service,
		std::string* mTextTwo);
	virtual ~NetworkTwoUIDialog();

	std::string* p_mTextTwo;

	enum NETWORK_TWO_IDS
	{
		ID_TEXTCTRL
	};

	void BuildPage();

	void SetText( wxCommandEvent& event );

	wxTextCtrl* mTextTwoCtrl;
	wxButton* mUpdateButton;

	ves::conductor::util::CORBAServiceList* mServiceList;

	std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }

	DECLARE_EVENT_TABLE();
};

#endif //NETWORK_TWO_PLUGIN_H