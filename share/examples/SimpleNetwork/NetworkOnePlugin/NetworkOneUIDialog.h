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
		std::string* mText);
	virtual ~NetworkOneUIDialog();

	std::string* p_mText;

	enum NETWORK_ONE_IDS
	{
		ID_TEXTCTRL
	};
	
	virtual bool TransferDataFromWindow();
	virtual bool TransferDataToWindow();

	void BuildPage();

	void SetText( wxCommandEvent& event );

	wxTextCtrl* mTextCtrl;
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