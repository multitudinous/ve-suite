#include "paraThread.h"
#include "Network.h"

paraThread::paraThread(Network* network) : wxThread(wxTHREAD_JOINABLE)
{
  nw = network;
}
paraThread::~paraThread()
{
}

bool paraThread::Do()
{
  if (nw->paraview == true)
    return false;
  //cout<<"starting paraview"<<endl;
  nw->paraview = true;
  system("paraview");
  nw->paraview = false;
  return true;
}
  
