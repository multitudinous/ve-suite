#ifndef _OPPD_UI_CONSTANTS_H_
#define _OPPD_UI_CONSTANTS_H_

#include <wx/wx.h>
#include <wx/notebook.h>


class OPPD_UI_constants: public wxPanel
{
public:


	OPPD_UI_constants(wxNotebook* parent);

	~OPPD_UI_constants();

	wxStaticText* _compcap;
	wxStaticText* _comptype;
	wxStaticText* _compspillarea;
	wxStaticText* _fwpumpcap;
	wxStaticText* _fwpumptype;
	wxStaticText* _fwpumpspillarea;
	wxStaticText* _roomlength;
	wxStaticText* _roomheight;
	wxStaticText* _roomwidth;
	
protected:
  void _buildPage();

};
#endif //_OPPD_UI_CONSTANTS_H_