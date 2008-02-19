#ifndef AGENT_H
#define AGENT_H

// --- OSG Includes --- //
#include <osg/Geode>

namespace Construction
{
class Agent : public osg::Geode
{
public:
	Agent();

protected:
	virtual ~Agent();

public:
    Agent( const Agent& agent, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    META_Node( Construction, Agent );

    void CreateAgent();

private:
    
    
};
}

#endif //AGENT_H
