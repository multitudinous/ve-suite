
///////////////////////////////////////////////////////////////////////////////
//
//  Redirect Minerva log statments to vprDBG.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_LOG_H
#define VES_XPLORER_MINERVA_LOG_H

#include "Usul/Base/Object.h"
#include "Usul/Interfaces/ILog.h"

namespace ves {
namespace xplorer {
namespace minerva {


class Log : 
  public Usul::Base::Object,
  public Usul::Interfaces::ILog
{
  typedef Usul::Base::Object BaseClass;

public:

  USUL_DECLARE_QUERY_POINTERS ( Log );
  USUL_DECLARE_IUNKNOWN_MEMBERS;

  Log();

  // Write the string.
  virtual void write ( const std::string &s, bool appendNewLine = true, bool prependEventCount = true );

protected:

  virtual ~Log();

  
};


}
}
}

#endif // VES_XPLORER_MINERVA_ADD_EARTH_HANDLER_H
