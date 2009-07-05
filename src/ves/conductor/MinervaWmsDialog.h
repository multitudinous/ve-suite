
///////////////////////////////////////////////////////////////////////////////
//
//  Dialog for WMS server properties.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef _MINERVA_WMS_DIALOG_H_
#define _MINERVA_WMS_DIALOG_H_

#include <wx/dialog.h>

#include <string>
#include <ves/VEConfig.h>

class wxTextCtrl;
class wxStaticText;
class wxRadioButton;
class wxCheckBox;
class wxStdDialogButtonSizer;
class wxButton;

class VE_GUIPLUGINS_EXPORTS MinervaWmsDialog : public wxDialog
{
public:
  typedef wxDialog BaseClass;

  MinervaWmsDialog ( wxWindow *parent, wxWindowID id );
  virtual ~MinervaWmsDialog();

  std::string server() ;
  std::string layers() ;
  std::string styles() ;
  std::string format() const;

private:

    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }

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
