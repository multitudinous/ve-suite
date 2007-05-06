
#include <osgDB/ReadFile>
#include <osg/Vec2>
#include <osg/Vec4>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/StateSet>
#include <osg/LineWidth>
#include <osg/Texture2D>
#include <osg/TexEnv>
#include <osg/AlphaFunc>
#include <osg/BlendFunc>
#include <osg/Node>
#include <osg/LOD>
#include <osg/Transform>
#include <osg/MatrixTransform>
#include <osg/Switch>
#include <osg/Group>
#include <osg/NodeVisitor>
#include <osgText/Text>

#include <iostream>
#include <vector>
#include <list>
#include <string>
using std::vector;
using std::list;
using std::string;

#include <cassert>



class BoundingBoxVisitor : public osg::NodeVisitor
{
public:
    BoundingBoxVisitor()
        : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN )
    {
        osg::Matrix m;
        _m.push_back( m );
    }
    ~BoundingBoxVisitor() {}

    osg::BoundingBox getBound() { return _bb; }

    virtual void apply( osg::Node& node )
    {
        traverse( node );
    }
    virtual void apply( osg::MatrixTransform& mt )
    {
        osg::Matrix m = mt.getMatrix();
        pushTraversePop( m, (osg::Node&)mt );
    }

    virtual void apply( osg::Geode& geode )
    {
        unsigned int i;
        for( i = 0; i < geode.getNumDrawables(); i++ )
        {
            osg::Geometry* geom = dynamic_cast<osg::Geometry *>(geode.getDrawable(i));
            if( !geom )
                continue;

            osg::BoundingBox bb = geom->getBound();
            osg::Matrix c = _m.back();
            osg::Vec3 v0 = bb._min * c;
            osg::Vec3 v1 = bb._max * c;
            _bb.expandBy( v0 );
            _bb.expandBy( v1 );
        } 
    }

protected:
    void pushTraversePop( const osg::Matrix& m, osg::Node& node )
    {
        osg::Matrix c = _m.back();
        _m.push_back( m*c );
        traverse( node );
        _m.pop_back();
    }

    osg::BoundingBox _bb;
    std::vector<osg::Matrix> _m;
};

class ChildInfo
{
public:
    ChildInfo() : _shared( false ), _sharedCount( 0 ) {}

    osg::Vec2 _attach;
    bool _shared;
    int _sharedCount;
    osg::ref_ptr<osg::Node> _node;
};
typedef list<ChildInfo> ChildList;


class TreeVisitor : public osg::NodeVisitor
{
public:
    TreeVisitor( bool nodeNames = true );
    virtual ~TreeVisitor();

    virtual void apply( osg::Node& node );

    void getExtents( osg::Vec2& minV, osg::Vec2& maxV ) { minV = _minV; maxV = _maxV; }

    osg::Node* getTree() { return _root.get(); }

protected:
    osg::Group* addNode( const osg::Vec4& color, const string& name );
    bool sharedChild( const ChildList& cl );

    void processCRs( string& in );

    osg::ref_ptr<osg::StateSet> _shadowState;
    osg::ref_ptr<osg::StateSet> _lineState;
    osg::ref_ptr<osg::StateSet> _buttonState;
    osg::ref_ptr<osg::StateSet> _textState;

    osg::ref_ptr<osg::Group> _root;
    osg::ref_ptr<osg::Group> _parent;

    osg::ref_ptr<osg::Geode> _buttonGeode;
    osg::ref_ptr<osg::Vec2Array> _texCoords;
    osg::ref_ptr<osg::Vec3Array> _shadowVerts;

    vector<float> _minX;
    float _maxX;

    int _level;
    vector<ChildList> _lChild;

    osg::Vec2 _curLoc;
    osg::Vec2 _minV, _maxV;

    // Should be static...
    osg::Vec2 _footprint;
    osg::Vec2 _size;
    osg::Vec2 _outOffset;
    osg::Vec2 _inOffset;
    osg::Vec2 _shadowOffset;

    osg::Vec4 _grey, _red, _orange, _yellow, _green, _cyan;

    bool _nodeNames;
};


void
TreeVisitor::processCRs( string& in )
{
    int idx = in.find( "\\n" );
    while (idx != in.npos)
    {
        in.replace( idx, 2, "\n" );
        idx = in.find( '\\' );
    }
}


TreeVisitor::TreeVisitor( bool nodeNames )
  : NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
    _nodeNames( nodeNames ),
    _level( 0 ),
    _maxX( 0.f )
{
    // Create state for rendering shadows
    _shadowState = new osg::StateSet;
    _shadowState->setRenderingHint( osg::StateSet::TRANSPARENT_BIN );

    osg::Texture2D* texture = new osg::Texture2D;
    texture->setImage( osgDB::readImageFile("Images/bShadow.tif") );
    _shadowState->setTextureAttributeAndModes( 0, texture, osg::StateAttribute::ON );

    osg::AlphaFunc* af = new osg::AlphaFunc;
    af->setFunction( osg::AlphaFunc::GEQUAL, 0.05f );
    _shadowState->setAttributeAndModes( af, osg::StateAttribute::ON );

    osg::BlendFunc* bf = new osg::BlendFunc;
    _shadowState->setAttributeAndModes( bf, osg::StateAttribute::ON );

    // Create state for rendering lines
    _lineState = new osg::StateSet;

    osg::LineWidth* lw = new osg::LineWidth( 3.f );
    _lineState->setAttributeAndModes( lw );

    // Create state for rendering the button
    _buttonState = new osg::StateSet;

    // Create state for rendering the text
    _textState = new osg::StateSet;


    _buttonGeode = dynamic_cast<osg::Geode*>( osgDB::readNodeFile( "gsphereoid.osg" ) );
    assert( _buttonGeode.valid() );


    _root = new osg::Group;
    _parent = _root.get();


    _minX.push_back( 0.f );

    _curLoc[0] = _curLoc[1] = 0.f;
    _minV[0] = _minV[1] = 1000.f;
    _maxV[0] = _maxV[1] = -1000.f;

    BoundingBoxVisitor bbv;
    _buttonGeode->accept( bbv );
    osg::Vec3 bbMin = bbv.getBound()._min;
    osg::Vec3 bbMax = bbv.getBound()._max;
    _footprint[0] = (bbMax[0]-bbMin[0]) * 1.2f;
    _footprint[1] = (bbMax[1]-bbMin[1]) * 1.8f;
    _size[0] = bbMax[0]-bbMin[0];
    _size[1] = bbMax[1]-bbMin[1];
    _inOffset[0] = _size[0] * .5f;
    _inOffset[1] = _size[1] - .025f;
    _outOffset[0] = _size[0] * .5f;
    _outOffset[1] = .025f;
    _shadowOffset[0] = .125f;
    _shadowOffset[1] = .1f;

    // Use pastels
    _grey[0] = _grey[1] = _grey[2] = .55f; _grey[3] = 1.f;
    _red[0] = 1.f; _red[1] = _red[2] = .5f; _red[3] = 1.f;
    _orange[0] = 1.f; _orange[1] = .6f; _orange[2] = .3f; _orange[3] = 1.f;
    _yellow[0] = _yellow[1] = .85f; _yellow[2] = .2f; _yellow[3] = 1.f;
    _green[0] = .2f; _green[1] = .8f; _green[2] = .2f; _green[3] = 1.f;
    _cyan[0] = .2f; _cyan[1] = _cyan[2] = .8f; _cyan[3] = 1.f;


    _texCoords = new osg::Vec2Array;
    _texCoords->push_back( osg::Vec2( 0.f, 0.f ) );
    _texCoords->push_back( osg::Vec2( 1.f, 0.f ) );
    _texCoords->push_back( osg::Vec2( 1.f, 1.f ) );
    _texCoords->push_back( osg::Vec2( 0.f, 1.f ) );

    _shadowVerts = new osg::Vec3Array;
    float sx(_shadowOffset[0]),  sy(_shadowOffset[1]);
    _shadowVerts->push_back( osg::Vec3( sx, -sy, -1.f ) );
    _shadowVerts->push_back( osg::Vec3( sx+_size[0], -sy, -1.f ) );
    _shadowVerts->push_back( osg::Vec3( sx+_size[0], -sy+_size[1], -1.f ) );
    _shadowVerts->push_back( osg::Vec3( sx, -sy+_size[1], -1.f ) );
}

TreeVisitor::~TreeVisitor()
{
}


bool
TreeVisitor::sharedChild( const ChildList& cl )
{
    ChildList::const_iterator it = cl.begin();
    while (it != cl.end())
    {
        ChildInfo ci = *it;
        if (ci._shared)
            return true;
        it++;
    }
    return false;
}

osg::Group*
TreeVisitor::addNode( const osg::Vec4& color, const string& name  )
{
    if (_curLoc[0] < _minV[0]) _minV[0] = _curLoc[0];
    if (_curLoc[1]+_size[1] > _maxV[1]) _maxV[1] = _curLoc[1]+_size[1];


    //
    // Text
    osgText::Text* text = new osgText::Text;
    text->setText( name );
    text->setPosition( osg::Vec3( _size[0]*.5, _size[1]*.5, 0.f ) );
    text->setAlignment( osgText::Text::CENTER_CENTER );
    text->setFont( "fonts/arialn.ttf" );
    text->setFontResolution( 32, 32 );
    text->setCharacterSize( .125f );
    text->setAxisAlignment( osgText::Text::XY_PLANE );
    text->setColor( osg::Vec4( 0.f, 0.f, 0.f, 1.f ) );

    osg::Geode* textGeode = new osg::Geode;
    textGeode->addDrawable( text );
    textGeode->setStateSet( _textState.get() );


    //
    // Button geometry
    assert( _buttonGeode.valid() );
    osg::Geode* buttonGeode = dynamic_cast<osg::Geode*>(
        _buttonGeode->clone( ~osg::CopyOp::DEEP_COPY_ARRAYS ) );

    osg::Geometry* buttonGeom = buttonGeode->getDrawable( 0 )->asGeometry();
    osg::Vec4Array* c = new osg::Vec4Array;
    buttonGeom->setColorArray( c );
    buttonGeom->setColorBinding( osg::Geometry::BIND_OVERALL );
    c->push_back( color );


    //
    // Shadow geometry
    osg::Geometry* shadowGeom = new osg::Geometry;
    shadowGeom->setVertexArray( _shadowVerts.get() );

    float sx(_shadowOffset[0]),  sy(_shadowOffset[1]);
    if (_curLoc[0]+sx+_size[0] > _maxV[0]) _maxV[0] = _curLoc[0]+sx+_size[0];
    if (_curLoc[1]-sy < _minV[1]) _minV[1] = _curLoc[1]-sy;

    c = new osg::Vec4Array;
    shadowGeom->setColorArray( c );
    shadowGeom->setColorBinding( osg::Geometry::BIND_OVERALL );
    c->push_back( osg::Vec4( 1.f, 1.f, 1.f, 1.f ) );

    shadowGeom->setTexCoordArray( 0, _texCoords.get() );

    shadowGeom->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, 4 ) );

    osg::Geode* shadowGeode = new osg::Geode;
    shadowGeode->addDrawable( shadowGeom );
    shadowGeode->setStateSet( _shadowState.get() );


    //
    // Line geometry -- Draw from children to current node
    //   Add a line segment for each child in _lChild.
    osg::Geode* lineGeode = new osg::Geode;
    ChildList cList = _lChild[ _level ];
    if (!cList.empty())
    {
        osg::Geometry* lineGeom = new osg::Geometry;
        osg::Vec3Array* v = new osg::Vec3Array;
        lineGeom->setVertexArray( v );

        ChildList::const_iterator it = cList.begin();
        while (it != cList.end())
        {
            ChildInfo ci = *it;
            v->push_back( osg::Vec3( _outOffset[0], _outOffset[1], -.5f ) );
            v->push_back( osg::Vec3( ci._attach[0]-_curLoc[0], ci._attach[1]-_curLoc[1], -.5f ) );
            it++;
        }

        c = new osg::Vec4Array;
        lineGeom->setColorArray( c );
        lineGeom->setColorBinding( osg::Geometry::BIND_OVERALL );
        c->push_back( osg::Vec4( 0.f, 0.f, 0.f, 0.f ) );

        lineGeom->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::LINES,
            0, cList.size() * 2 ) );

        lineGeode->addDrawable( lineGeom );
        lineGeode->setStateSet( _lineState.get() );
    }


    osg::MatrixTransform* top = new osg::MatrixTransform;
    osg::Matrix m;
    m.makeTranslate( osg::Vec3( _curLoc[0], _curLoc[1], 0.f ) );
    top->setMatrix( m );

    if (!cList.empty())
        top->addChild( lineGeode );
    top->addChild( shadowGeode );
    top->addChild( buttonGeode );
    top->addChild( textGeode );

    return top;
}


void
TreeVisitor::apply( osg::Node& node )
{
    // If this is a shared child, and we've already visited it, just return.
    unsigned int numP( 1 );
    const bool shared = (node.getNumParents() > 1);
    if (shared)
    {
        assert( _level > 0 ); // can't be shared at root node level

        // Look for the current node address on the next level's child list.
        //  (Yes, NEXT level -- we haven't incremented 'level' yet).
        ChildList cList = _lChild[ _level-1 ];
        ChildList::const_iterator it = cList.begin();
        while (it != cList.end())
        {
            ChildInfo ci = *it;
            if (ci._node == &node)
            {
                _maxX += _footprint[0];
                return;
            }
            it++;
        }
        numP = node.getNumParents();
    }


    osg::Group* current = new osg::Group;
    _parent->addChild( current );
    _parent = current;

    string name( node.getName() );
    processCRs( name );
    osg::Vec4 color;
    unsigned int numC( 0 );
    osg::Group* grp = dynamic_cast<osg::Group*>( &node );
    if (grp)
        numC = grp->getNumChildren();

    if (osg::LOD* lod = dynamic_cast<osg::LOD*>( &node ))
    {
        if (_nodeNames )
            name = "LOD\n" + name;
        color = _grey;
    }
    else if (osg::Transform* trans = dynamic_cast<osg::Transform*>( &node ))
    {
        if (_nodeNames )
            name = "Transform\n" + name;
        color = _red;
    }
    else if (osg::Switch* sw = dynamic_cast<osg::Switch*>( &node ))
    {
        if (_nodeNames )
            name = "Switch\n" + name;
        color = _orange;
    }
    else if (osg::Geode* geode = dynamic_cast<osg::Geode*>( &node ))
    {
        if (_nodeNames )
            name = "Geode\n" + name;
        color = _yellow;
    }
    else if (grp)
    {
        if (_nodeNames )
            name = "Group\n" + name;
        color = _green;
    }
    else
    {
        // Unknown node type
        if (_nodeNames )
            name = "Unknown\n" + name;
        color = _cyan;
    }



    // Increment level.
    assert( _level >= 0 );
    _level++;
    _minX.push_back( _maxX );
    // Grow the vector of ChildList
    _lChild.resize( _level );
    traverse( node );
    _level--;



    if (numC > 0)
    {
        // Determine _curLoc[0] for current node, then pop _minX.
        const float minX = _minX.back();
        _curLoc[0] = (_maxX-minX) * .5f + minX - (_footprint[0]*.5f);
    }
    else
        _curLoc[0] = _maxX;
    _curLoc[1] = -( _footprint[1] * (float)_level );
    _minX.pop_back();

    if ( shared)
    {
        // Need to adjust X position to center between X width of all parents.
        //   Haven't computed parent location yet, so take a guess and assume
        //   X width = numP * _footprint[0]. This works for trivial cases.
        const float w = numP * _footprint[0];
        _curLoc[0] = (w-_maxX) * .5f + _maxX - (_footprint[0]*.5f);
    }

    if (_level > 0)
    {
        // Add the current node ChildInfo to the current level ChildList.
        //   This provides an attach point to connect the lines, and enables correct
        //   rendering if the current node has multiple parents.
        ChildInfo ci;
        ci._attach = osg::Vec2( _curLoc[0]+_inOffset[0], _curLoc[1]+_inOffset[1] );
        if (ci._shared = shared)
        {
            ci._node = &node;
            ci._sharedCount = node.getNumParents();
        }
        _lChild[_level-1].push_back( ci );
    }


    current->insertChild( 0, addNode( color, name ) );
    _parent = current->getParent( 0 );

    // If there's no shared child, resize to current level
    volatile unsigned int sz = _lChild.size();
    if (_lChild.size() > (unsigned int)_level)
    {
        if (sharedChild( _lChild[ _level ] ))
        {
            ChildInfo& ci = _lChild[ _level ].front();
            if (--(ci._sharedCount) == 0)
                _lChild.resize( _level );
        }
        else
            _lChild.resize( _level );
    }


    if (numC == 0)
        _maxX += _footprint[0];
}

osg::Node*
createTreeView( osg::Node* top, bool nodeNames )
{
    TreeVisitor tree( nodeNames );
    top->accept( tree );


    osg::Camera* camera = new osg::Camera;
    osg::StateSet* stateset = camera->getOrCreateStateSet();
    stateset->setMode( GL_LIGHTING, osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );

    osg::Vec2 minV, maxV;
    tree.getExtents( minV, maxV );

#if 0
    float xCen = (maxV[0]-minV[0]) *.5f + minV[0];
    float yCen = (maxV[1]-minV[1]) *.5f + minV[1];
    float xExt = 640.f/150.f;
    float yExt = 512.f/150.f;
    camera->setProjectionMatrix( osg::Matrix::ortho2D(
        xCen-xExt,  xCen+xExt,
        yCen-yExt,  yCen+yExt ) );
    camera->setViewMatrix( osg::Matrix::identity() );
#else
    float eyeDis( 30.f );
    float yUnits( 7.87f*.5f );
    double fovy( atan( yUnits/eyeDis ) * 360. / 3.1415927 );
    camera->setProjectionMatrix( osg::Matrix::perspective(
        fovy, 1280./1024., 10., 100. ) );

    float xCen = (maxV[0]-minV[0]) *.5f + minV[0];
    float yCen = (maxV[1]-minV[1]) *.5f + minV[1];
    osg::Matrix m;
    m.makeTranslate( osg::Vec3( -xCen, -yCen, -eyeDis ) );
    camera->setViewMatrix( m );
#endif

    camera->setReferenceFrame( osg::Transform::ABSOLUTE_RF );
    camera->setClearMask( GL_DEPTH_BUFFER_BIT );
    camera->setRenderOrder( osg::Camera::POST_RENDER );
    camera->addChild( tree.getTree() );

    return camera;
}

