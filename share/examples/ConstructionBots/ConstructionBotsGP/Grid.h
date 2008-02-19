#ifndef GRID_H
#define GRID_H

// --- OSG Libraries --- //
#include <osg/Geode>

// --- C/C++ Libraries --- //
//#include <vector>

namespace Construction
{
class Grid : public osg::Geode
{
public:
    Grid();
    Grid( int gridSize, int gridScale, std::vector< bool > occMatrix );

protected:
    virtual ~Grid();

public:
    Grid( const Grid& grid, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );
   
    META_Node( Construction, Grid );
   	
    void CreateGrid( int gridSize, int gridScale, std::vector< bool > occMatrix );

private:

};
}

#endif //GRID_H
