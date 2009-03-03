#ifndef NETWORK_THREE_UNIT_H
#define NETWORK_THREE_UNIT_H

#include <ves/open/moduleS.h>

#include <ves/ce/unitwrapper/UnitWrapper.h>

namespace ves
{
namespace ce
{
namespace unitwrapper
{
   class EventHandler;
}
}
namespace open
{
namespace xml
{
namespace model
{
   class Model;
}
}
}
}

class Body_Unit_i : public UnitWrapper
{
public:
	Body_Unit_i (Body::Executive_ptr exec, std::string name );
	virtual ~Body_Unit_i (void);

protected:
	void error(std::string msg);
	void warning(std::string msg);

    std::vector< std::string > mNetworkThreeInputs;

public:
	virtual void StartCalc( ACE_ENV_SINGLE_ARG_DECL )
	ACE_THROW_SPEC (( CORBA::SystemException, Error::EUnknown ));
};

#endif //NETWORK_THREE_UNIT
