
///////////////////////////////////////////////////////////////////////////////
//
//  Dialog for WMS server properties.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef _MINERVA_WMS_DIALOG_H_
#define _MINERVA_WMS_DIALOG_H_

#include <wx/dialog.h>

#include <string>

class wxTextCtrl;
class wxStaticText;
class wxRadioButton;
class wxCheckBox;
class wxStdDialogButtonSizer;
class wxButton;

class MinervaWmsDialog : public wxDialog
{
public:
  typedef wxDialog BaseClass;

  MinervaWmsDialog ( wxWindow *parent, wxWindowID id );
  virtual ~MinervaWmsDialog();

  std::string server() const;
  std::string layers() const;
  std::string styles() const;
  std::string format() const;

private:

	wxTextCtrl* _serverTextCtrl;
	wxTextCtrl* _layersTextCtrl;
	wxTextCtrl* _stylesTextCtrl;
	wxRadioButton* _jpegRadioBtn;
	wxRadioButton* _pngRadioBtn;
	wxRadioButton* _tiffRadioBtn;
	wxCheckBox* _transparentCheckBox;
	wxStdDialogButtonSizer* _sdbSizer;
	wxButton* _sdbSizerOK;
	wxButton* _sdbSizerCancel;
};

#endif // _MINERVA_WMS_DIALOG_H_
