#ifndef GETVESUITE_HeatX_HeatX_NATURA-1_H
#define GETVESUITE_HeatX_HeatX_NATURA-1_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_HeatX_HeatX_NATURA-1( void )
{
    unsigned char osgData[ 5002 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,54,0,150,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,253,235,255,0,130,142,127,193,47,255,0,97,143,219,207,254,10,141,251,36,73,251,87,252,14,255,0,133,172,218,207,236,7,251,103,174,164,127,225,101,252,95,240,55,218,71,192,63,218,7,246,41,147,225,55,31,13,188,127,163,249,63,217,47,251,82,124,118,63,187,217,246,255,0,248,78,177,169,253,176,105,154,63,246,127,93,255,0,6,243,126,202,95,3,63,102,175,216,103,196,26,191,193,111,12,120,131,194,18,124,80,253,167,127,108,75,111,25,105,87,63,19,62,41,120,183,194,183,111,240,79,246,196,253,161,126,6,124,60,212,244,111,5,248,215,198,154,142,145,225,15,16,67,240,159,192,158,7,209,117,61,79,71,177,176,212,124,69,109,224,205,38,79,17,93,106,183,58,109,156,240,255,0,60,150,255,0,240,116,47,143,62,51,254,210,191,2,255,0,106,13,35,254,9,251,225,29,30,47,134,159,179,199,237,5,240,98,15,4,234,63,182,94,179,44,186,244,127,181,31,142,127,101,79,31,218,248,158,95,16,218,254,200,76,52,135,209,87,246,101,22,178,88,139,43,177,126,124,109,231,45,237,167,246,103,149,127,223,254,195,95,240,95,207,140,223,178,191,194,143,9,254,204,86,223,176,191,195,31,136,26,142,163,226,239,218,231,227,181,159,139,167,253,179,188,85,225,11,35,167,252,81,253,169,252,85,241,155,83,240,245,206,144,159,177,158,170,240,222,105,151,127,180,22,151,165,65,58,79,42,106,49,248,114,227,81,146,61,53,167,143,79,78,8,206,113,199,87,148,234,91,14,225,30,84,229,167,50,187,151,187,125,52,181,217,86,188,85,150,183,127,141,173,253,121,159,221,221,21,252,179,233,127,240,112,127,237,55,171,74,98,182,255,0,130,122,124,9,70,18,67,25,51,126,223,255,0,16,20,110,157,153,80,229,63,224,159,13,198,84,230,189,35,75,255,0,130,223,254,215,90,190,13,183,236,1,251,56,38,99,121,63,127,255,0,5,8,248,154,188,36,130,34,63,119,255,0,4,232,111,155,113,7,233,223,181,110,241,120,101,189,120,175,152,114,203,177,253,40,81,95,207,190,153,255,0,5,110,253,182,181,98,5,191,236,25,251,44,33,46,241,254,251,254,10,33,241,105,112,201,16,148,231,203,255,0,130,108,183,27,72,252,123,87,125,166,127,193,74,191,111,77,83,111,145,251,11,126,200,137,187,201,199,155,255,0,5,21,248,202,191,235,254,230,118,255,0,193,50,207,167,53,15,31,130,91,226,97,247,135,44,187,31,185,84,87,227,118,159,251,117,255,0,193,67,53,32,26,15,216,131,246,50,64,85,223,247,191,240,81,175,141,234,112,146,121,103,238,127,193,47,219,157,221,61,171,172,178,253,174,127,224,163,247,219,60,175,216,159,246,35,79,48,197,143,55,254,10,65,241,217,79,239,185,76,133,255,0,130,91,30,221,127,76,212,60,207,47,91,226,224,175,230,62,73,255,0,43,63,73,190,44,124,81,240,39,192,239,133,159,18,254,53,124,81,215,127,225,23,248,101,240,127,225,255,0,140,190,40,252,69,241,47,246,102,177,173,255,0,194,59,224,79,135,254,29,212,188,89,226,237,119,251,27,195,186,125,222,161,171,253,147,195,250,78,161,113,246,91,27,75,171,203,143,179,249,86,182,243,78,233,27,97,205,241,215,225,92,31,27,188,63,251,57,75,226,157,159,25,124,83,240,171,198,31,27,116,31,6,255,0,98,120,141,190,223,240,199,192,94,43,240,23,130,60,89,226,111,248,72,87,72,58,85,183,217,60,81,241,59,192,246,191,98,154,250,61,66,127,237,191,58,218,210,107,123,107,201,109,255,0,40,255,0,106,61,83,254,10,103,251,87,126,203,31,180,175,236,213,15,236,173,251,10,120,20,126,209,31,179,239,198,111,129,255,0,240,154,203,255,0,5,9,248,255,0,226,67,224,241,241,95,225,239,136,254,31,31,20,127,194,54,191,240,76,43,31,248,72,14,158,124,65,246,191,176,253,190,203,237,98,211,200,251,101,183,153,231,38,22,165,224,79,248,41,243,254,219,63,15,127,108,151,253,155,63,96,196,30,6,253,149,254,47,254,204,99,225,194,255,0,193,64,127,104,55,58,175,252,45,143,138,191,1,254,39,255,0,194,111,255,0,9,121,255,0,130,99,175,216,133,135,252,40,255,0,176,157,55,251,50,111,181,127,194,79,246,159,183,219,125,136,219,221,243,212,205,176,202,190,30,52,241,20,165,66,92,254,210,78,74,241,178,92,150,87,87,187,109,61,31,200,106,156,185,100,218,105,171,89,91,115,247,82,138,252,182,190,253,166,191,224,165,118,0,153,191,98,191,216,105,128,85,111,221,127,193,72,190,62,30,29,202,15,191,255,0,4,176,94,227,242,53,202,93,254,217,95,240,81,139,55,9,39,236,73,251,20,49,45,34,130,159,240,81,239,142,100,102,50,1,39,127,252,18,232,113,200,197,111,253,169,151,59,127,182,67,95,49,114,79,249,89,250,233,69,126,44,234,31,240,80,47,248,40,30,154,164,205,251,14,126,199,15,133,87,196,95,240,81,159,141,103,135,147,203,28,191,252,19,17,121,221,250,87,9,169,127,193,80,255,0,110,173,47,204,251,71,236,37,251,38,191,151,231,238,242,127,224,162,127,24,27,63,103,63,62,55,255,0,193,52,87,240,171,89,134,9,237,138,131,249,175,235,168,156,36,183,139,71,239,13,21,252,239,106,127,240,88,175,219,59,74,102,75,143,216,35,246,96,114,174,136,124,159,248,40,111,197,86,27,164,143,204,31,127,254,9,190,188,109,206,125,199,74,243,93,83,254,11,187,251,87,105,40,210,92,127,193,63,63,103,169,2,164,110,68,31,240,80,79,137,46,196,73,47,148,0,15,255,0,4,238,92,157,223,160,252,42,150,55,9,47,135,17,23,243,14,89,118,63,166,186,43,249,74,213,63,224,227,15,218,59,72,14,110,191,224,158,31,4,223,101,203,90,159,35,246,252,241,211,230,69,18,18,71,153,251,0,47,201,251,182,231,175,35,142,184,249,238,203,254,14,196,248,169,125,225,31,14,120,210,31,248,38,215,195,245,210,252,81,225,157,3,197,90,124,18,254,221,190,35,91,248,180,239,17,90,219,93,216,195,121,18,126,195,173,26,94,36,119,72,37,88,229,146,53,101,33,36,112,50,116,88,138,50,87,85,83,94,162,105,173,209,253,156,209,95,194,55,196,15,248,61,71,93,248,111,115,105,107,174,127,193,49,116,155,169,47,39,212,173,226,109,43,246,224,188,157,3,233,114,91,71,112,206,111,63,99,136,8,66,215,81,236,192,36,237,109,193,112,50,86,169,166,147,78,233,136,254,77,63,102,225,255,0,20,247,133,184,31,242,6,240,39,110,198,196,31,78,123,231,250,226,190,252,240,89,255,0,139,207,240,208,158,159,240,167,62,57,123,224,127,194,192,248,11,131,215,145,244,252,184,175,238,231,77,255,0,131,108,191,224,140,154,52,113,67,163,254,202,30,46,210,162,129,45,227,130,45,55,246,189,253,183,44,99,134,59,85,219,107,28,73,109,251,71,168,141,35,94,35,0,0,131,133,192,174,130,31,248,55,123,254,9,29,109,123,107,169,91,254,206,223,19,173,245,27,43,43,237,54,206,254,15,219,95,246,238,138,246,211,79,213,46,52,251,189,78,194,214,234,63,218,96,60,22,119,55,122,86,151,44,241,35,4,154,77,54,221,228,86,104,99,43,199,60,35,156,156,185,237,123,233,110,234,197,41,89,90,221,79,229,179,193,32,27,215,227,254,94,180,225,245,38,89,120,233,234,77,125,129,224,156,237,94,255,0,232,183,61,58,144,46,215,166,7,62,159,133,126,158,127,193,59,255,0,224,141,31,240,79,239,141,31,176,63,236,63,241,183,226,79,128,190,59,248,131,226,127,198,31,217,7,246,103,248,167,241,27,196,240,126,220,127,183,95,135,207,136,188,123,227,223,131,30,11,241,111,138,245,239,236,127,12,254,210,86,122,126,144,110,188,67,171,234,55,2,218,198,210,214,206,223,237,62,85,173,188,16,170,70,191,107,193,255,0,4,56,255,0,130,113,218,140,91,120,7,246,143,183,24,43,251,143,248,40,79,252,20,66,47,149,142,226,63,119,251,84,142,11,115,245,231,173,121,245,50,121,77,91,219,37,242,126,94,101,251,69,216,252,170,240,103,46,157,63,227,230,227,191,113,102,167,29,63,206,43,234,95,6,231,247,35,142,127,179,113,239,157,222,157,57,38,190,200,131,254,8,179,251,1,91,115,109,225,223,218,154,3,146,217,135,254,10,57,255,0,5,29,143,12,70,210,70,207,218,192,96,149,224,251,113,90,176,255,0,193,29,191,97,219,124,125,158,215,246,186,131,27,113,228,255,0,193,74,255,0,224,164,241,227,103,220,198,207,218,212,116,237,233,218,185,167,195,243,149,255,0,218,82,189,190,203,242,243,245,5,81,38,157,182,60,227,194,195,247,113,231,159,220,79,207,210,232,118,252,63,74,247,111,14,31,248,247,57,31,242,227,142,125,189,49,207,111,206,176,226,255,0,130,72,126,198,176,99,200,188,253,179,97,192,32,121,95,240,83,175,248,41,140,120,4,228,129,179,246,186,28,19,87,227,255,0,130,82,254,201,16,227,201,215,191,109,184,182,237,219,229,255,0,193,81,127,224,167,41,141,191,119,27,127,107,225,140,118,244,174,119,195,85,27,191,214,163,255,0,128,191,243,52,246,235,249,127,19,232,111,13,255,0,171,92,103,253,76,167,142,163,253,32,100,231,60,99,61,190,149,232,122,143,16,47,111,222,129,131,199,240,72,58,26,252,198,253,171,63,224,151,94,18,135,246,88,253,165,71,236,175,226,255,0,219,174,47,218,100,126,207,255,0,25,7,236,232,35,255,0,130,168,255,0,193,68,83,254,47,169,248,119,226,51,240,140,15,248,88,63,182,16,240,254,63,225,96,127,194,63,255,0,33,223,248,147,127,208,79,253,11,207,174,66,247,254,9,139,168,191,237,181,240,249,109,252,115,251,121,191,236,102,63,101,111,140,7,199,97,191,224,171,31,240,80,6,31,240,211,195,226,183,192,97,240,152,226,95,219,19,254,18,172,255,0,194,175,63,28,127,227,207,254,41,254,79,246,151,252,76,63,178,43,150,124,51,82,53,105,83,250,195,151,181,230,247,148,27,140,121,82,126,251,190,156,219,71,71,119,125,134,171,38,155,229,218,218,95,115,239,95,18,31,221,191,253,113,139,243,55,7,250,159,214,188,59,92,255,0,94,188,241,230,220,127,232,105,237,215,252,43,164,147,254,9,99,251,42,205,196,222,41,253,185,101,4,0,124,207,248,42,111,252,20,249,248,7,32,124,223,182,9,227,60,213,9,63,224,147,191,178,20,188,203,172,126,218,210,28,147,153,63,224,168,63,240,83,119,228,245,63,55,237,122,121,61,235,161,112,197,93,63,218,226,173,218,47,252,201,246,235,249,127,19,230,79,21,115,27,250,121,16,146,123,28,220,245,60,115,219,154,249,147,197,255,0,242,245,206,48,117,60,159,76,30,115,249,15,207,218,191,73,165,255,0,130,67,254,197,147,130,39,147,246,198,152,16,1,18,255,0,193,77,63,224,165,178,112,14,224,62,127,218,224,241,187,159,175,53,153,55,252,17,179,246,19,184,207,218,52,207,218,202,125,219,183,121,223,240,82,95,248,41,28,185,223,247,243,191,246,179,57,207,127,94,245,188,120,118,162,122,226,147,255,0,183,95,151,152,165,91,154,222,237,172,126,33,120,207,253,116,160,255,0,207,197,191,29,49,254,135,192,246,226,190,57,241,175,252,123,205,255,0,94,246,220,250,127,166,117,62,158,156,122,227,189,127,77,147,127,193,19,127,224,158,215,36,155,143,9,254,211,215,4,144,73,155,254,10,45,255,0,5,25,148,146,23,104,36,201,251,87,28,144,188,125,56,172,121,191,224,133,127,240,77,107,144,69,199,195,111,218,26,224,16,1,19,255,0,193,64,191,224,161,114,130,1,220,160,137,63,106,115,192,110,71,161,231,173,116,195,37,156,29,253,186,183,248,127,224,250,147,237,23,99,248,230,241,199,2,124,241,255,0,19,121,125,59,11,191,243,248,226,191,39,124,57,255,0,36,19,225,7,253,145,175,133,159,137,58,30,145,239,199,255,0,91,215,138,254,233,60,121,255,0,4,90,255,0,130,120,217,126,223,63,179,87,193,72,62,27,124,109,255,0,133,101,241,7,246,67,253,184,126,42,248,191,195,51,254,219,127,183,53,223,246,191,143,254,16,124,104,255,0,130,124,120,79,225,215,136,14,179,119,251,72,62,161,96,250,119,135,254,57,252,84,183,251,53,173,220,54,151,159,240,149,25,111,237,238,167,178,211,164,180,224,254,32,255,0,193,8,255,0,224,151,158,26,253,184,255,0,101,191,128,90,31,192,79,136,58,127,194,15,24,126,198,223,182,215,143,245,255,0,4,69,251,91,254,217,146,88,92,120,167,224,127,197,175,248,39,231,132,190,21,222,217,222,79,251,65,53,222,141,101,164,120,119,227,87,196,187,104,172,108,174,45,236,39,95,17,135,188,181,184,150,199,78,146,211,162,120,88,224,232,85,173,82,167,52,105,218,78,209,214,203,178,186,239,220,92,220,205,43,106,244,251,236,127,154,47,237,89,255,0,33,157,11,254,194,126,49,245,29,46,180,113,158,156,142,180,87,250,94,255,0,193,27,255,0,224,141,223,240,76,127,19,254,206,22,127,180,55,196,63,217,3,225,175,198,111,138,186,199,198,15,219,155,225,69,230,169,251,64,94,120,203,246,133,240,196,94,10,248,115,251,116,124,107,248,117,224,184,52,207,134,191,27,252,79,175,248,99,68,241,21,143,130,62,21,120,31,77,93,118,203,71,183,215,102,183,211,110,69,198,165,51,234,154,171,95,21,234,82,86,167,13,111,117,127,191,83,51,250,136,162,138,252,233,248,251,255,0,5,22,209,254,10,126,212,22,159,178,55,133,191,101,143,218,127,246,140,248,175,55,193,239,1,252,108,189,147,224,165,199,236,185,163,248,91,71,240,167,196,143,26,252,88,240,31,133,236,53,13,99,246,132,253,167,124,7,44,222,32,147,88,248,51,227,41,38,138,210,218,234,218,27,97,104,239,119,230,78,97,142,167,56,83,139,157,73,168,66,59,182,210,75,213,189,16,26,255,0,240,73,223,249,69,151,252,19,79,254,204,3,246,55,255,0,214,117,248,115,95,127,215,225,31,252,19,71,246,220,211,126,15,120,71,246,10,255,0,130,92,126,209,127,179,207,199,255,0,217,219,246,144,240,159,236,175,240,207,224,126,137,172,120,250,127,217,247,198,159,11,62,36,124,81,253,156,127,103,221,34,111,31,248,95,193,94,38,248,5,241,251,198,119,246,62,111,133,62,30,120,251,94,211,111,124,69,164,104,26,117,198,157,225,57,173,166,187,181,214,238,244,189,30,255,0,247,114,136,78,21,34,167,78,106,164,30,206,45,52,254,107,64,10,40,162,168,2,138,40,160,2,138,40,160,2,138,40,160,2,138,40,160,2,138,43,159,241,103,139,60,43,224,47,10,248,155,199,94,58,241,55,135,252,23,224,159,5,248,127,89,241,103,140,124,99,226,205,103,78,240,231,133,124,39,225,95,14,105,215,58,199,136,124,77,226,111,16,235,23,48,218,104,62,31,176,210,44,239,46,175,111,110,166,138,218,214,218,214,73,231,145,34,70,96,1,241,7,196,111,249,74,111,236,111,255,0,102,1,255,0,5,44,255,0,214,138,255,0,130,78,215,199,127,181,223,252,19,115,254,10,69,241,215,246,199,159,246,166,253,159,191,224,176,31,240,203,30,29,240,239,128,60,77,240,187,224,223,194,63,248,119,247,192,127,142,31,240,170,188,11,241,55,78,248,19,125,241,171,67,255,0,132,247,198,222,58,179,159,199,63,240,147,124,74,248,1,224,239,16,253,171,85,211,164,188,209,182,127,100,233,55,22,246,6,232,94,226,120,247,254,10,109,255,0,4,219,188,255,0,130,145,254,202,30,58,179,255,0,130,131,126,196,23,126,9,240,231,236,65,255,0,5,6,240,159,136,124,99,109,251,87,252,6,159,194,186,15,138,124,105,241,231,254,9,149,172,120,59,195,58,207,136,34,241,241,180,210,252,65,171,105,30,2,241,213,214,153,103,60,201,115,127,109,224,173,90,123,88,229,139,78,188,104,127,111,188,39,226,207,10,248,247,194,190,25,241,215,129,124,77,225,255,0,26,120,39,198,158,31,209,188,89,224,239,24,248,79,89,211,188,71,225,95,22,120,87,196,122,117,182,177,225,239,19,120,103,196,58,61,204,214,154,247,135,239,244,139,203,59,171,43,219,89,165,182,186,182,186,142,120,36,120,157,88,204,163,25,197,198,113,83,140,183,77,93,63,84,244,13,182,62,42,255,0,130,109,126,200,191,20,127,97,191,217,115,74,253,157,126,43,252,125,240,255,0,237,45,174,104,95,18,190,50,248,250,207,226,182,141,240,92,252,10,212,117,132,248,219,241,59,196,255,0,26,188,85,15,139,60,35,109,241,63,196,246,23,190,32,31,18,252,127,227,201,163,188,210,155,68,211,151,74,190,211,52,216,244,72,167,211,174,53,29,76,175,189,104,170,73,37,100,172,144,5,126,57,126,209,223,178,167,237,160,255,0,240,81,120,127,108,207,217,211,193,191,179,7,196,159,4,94,254,203,223,4,62,9,234,190,20,248,215,251,68,252,86,248,19,226,171,15,21,124,39,248,179,251,77,120,242,251,80,211,207,129,127,100,223,136,246,122,166,129,121,164,124,115,208,226,138,89,39,180,185,138,227,70,187,71,180,104,154,25,92,162,179,173,70,157,122,114,165,86,60,244,231,186,187,87,179,190,233,167,186,26,109,59,173,26,60,75,64,253,133,191,111,175,136,159,240,82,223,217,67,246,203,248,213,224,223,217,3,225,119,195,47,128,94,48,248,195,226,191,19,104,31,11,127,105,239,141,31,26,252,119,172,127,194,193,253,155,190,50,252,24,209,172,52,61,55,197,159,177,127,128,180,247,49,120,131,226,78,149,117,117,45,198,167,6,203,59,27,131,20,115,79,229,195,39,239,213,20,82,163,66,150,30,156,104,209,135,37,56,108,174,221,190,246,216,54,219,187,119,108,40,162,138,212,65,69,20,80,1,69,20,80,1,69,20,80,1,69,20,80,1,95,0,127,193,88,127,229,22,95,240,82,188,244,255,0,134,0,253,178,63,245,157,126,35,81,69,0,127,29,167,254,14,154,255,0,130,125,143,217,47,254,20,79,252,41,239,219,31,254,18,225,23,150,117,31,248,87,223,5,63,225,27,220,33,242,127,227,243,254,26,23,237,88,221,255,0,78,121,197,127,74,95,240,110,79,138,180,239,29,127,193,30,191,102,63,27,233,16,222,219,233,94,49,241,247,237,149,226,173,50,223,82,142,8,117,24,52,255,0,16,254,220,191,180,158,175,103,13,252,86,215,51,71,21,234,219,222,70,178,172,115,74,138,234,193,36,117,1,137,69,121,121,117,10,84,93,103,78,60,174,111,93,91,252,219,52,168,219,181,250,105,249,31,183,244,81,69,122,134,103,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 5002; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

