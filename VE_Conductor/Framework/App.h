#ifndef REIAPP_H
#define REIAPP_H

#ifdef WIN32
#include <winsock2.h>
#endif
#include <wx/wx.h>

class AppFrame;

class REIApp : public wxApp
{
  

 public:
  virtual bool OnInit();
  virtual int OnExit();

  AppFrame *mainFrame;
  
  long id;
};

DECLARE_APP(REIApp)

#endif
