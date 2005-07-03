#ifndef UIDIALOG_H
#define UIDIALOG_H

#include <wx/wx.h>
#include <wx/notebook.h>

#ifdef WIN32
#pragma warning(disable : 4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#endif

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
