#ifndef UIDIALOG_H
#define UIDIALOG_H

#include <wx/wx.h>
#include <wx/notebook.h>

#ifdef WIN32
#pragma warning(disable : 4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#endif

#include "VE_Xplorer/cfdConfig.h"

class WXPLUGIN_DECLSPEC UIDialog : public wxDialog
{
  //DECLARE_DYNAMIC_CLASS(UIDialog)
    
 public:
  UIDialog() {};
  UIDialog(wxWindow* parent, int id, wxString title="UI");
  virtual void Lock(bool l); 
  //This function locks/unlocks every input entry

 protected:
  bool lock;
};

#endif
