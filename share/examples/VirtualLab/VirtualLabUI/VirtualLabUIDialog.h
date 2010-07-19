
#ifndef VIRTUAL_LAB_UI_DIALOG_H
#define VIRTUAL_LAB_UI_DIALOG_H

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


class VirtualLabUIDialog : public ves::conductor::UIDialog
{	
public:
	VirtualLabUIDialog( wxWindow* parent, int id, 
		ves::conductor::util::CORBAServiceList* service,
		std::string* mTextOne);
	virtual ~VirtualLabUIDialog();

	std::string* p_mTextOne;

	enum VIRTUAL_LAB_IDS
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

#endif

