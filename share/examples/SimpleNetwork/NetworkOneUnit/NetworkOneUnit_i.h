#ifndef NETWORK_ONE_UNIT_H
#define NETWORK_ONE_UNIT_H

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
    Body_Unit_i(Body::Executive_ptr exec, std::string name );
    virtual ~Body_Unit_i();

protected:
    void error(std::string msg);
    void warning(std::string const msg);

    std::string mTextOne;

public:
    virtual void StartCalc();
};

#endif //NETWORK_ONE_UNIT
