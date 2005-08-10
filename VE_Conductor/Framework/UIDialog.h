#ifndef UIDIALOG_H
#define UIDIALOG_H

#include <wx/wx.h>
#include <wx/notebook.h>

#include "VE_Installer/include/VEConfig.h"

class VE_GUIPLUGINS_EXPORTS UIDialog : public wxDialog
{
   public:
      UIDialog() {;}
      UIDialog(wxWindow* parent, int id, wxString title="UI");
      virtual void Lock(bool l); 
      //This function locks/unlocks every input entry

   protected:
      bool lock;
};

#endif
