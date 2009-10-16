
#include <ves/xplorer/minerva/Log.h>

#include <ves/xplorer/Debug.h>

using namespace ves::xplorer::minerva;

USUL_IMPLEMENT_IUNKNOWN_MEMBERS ( Log, Log::BaseClass )

///////////////////////////////////////////////////////////////////////////////
Log::Log() : BaseClass()
{
}
///////////////////////////////////////////////////////////////////////////////
Log::~Log()
{
}
///////////////////////////////////////////////////////////////////////////////
Usul::Interfaces::IUnknown* Log::queryInterface ( unsigned long iid )
{
  switch ( iid )
  {
  case Usul::Interfaces::IUnknown::IID:
  case Usul::Interfaces::ILog::IID:
    return static_cast<Usul::Interfaces::ILog*> ( this );
  default:
    return 0x0;
  }
}
///////////////////////////////////////////////////////////////////////////////
void Log::write ( const std::string &s, bool appendNewLine, bool prependEventCount )
{
  vprDEBUG( vesDBG, 1 ) << s << std::endl << vprDEBUG_FLUSH;
}
