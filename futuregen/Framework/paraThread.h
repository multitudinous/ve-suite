#ifndef PARATHREAD
#define PARATHREAD

#include <wx/thread.h>
#include <string>

class Network;

class paraThread : public wxThread  
{
public:
	paraThread(Network* nt);
	virtual ~paraThread();
	bool Do();
	virtual ExitCode Entry() { return (ExitCode) this->Do(); };

 protected:
	Network* nw;
};

#endif
