--newPackage(
--    "MRDIJSON",
--    Headline => "MRDI file format",
--    Version => "0.1",
--    Date => "November 25, 2024",
--    Authors => {{
--	    Name => "Antony Della Vecchia",
--	    Email => "vecchia@math.tu-berlin.de",
--	    HomePage => "https://antonydellavecchia.github.io"}},
--    Keywords => {"System"},
--    PackageImports => {"JSON"},
--    AuxiliaryFiles => true)
--
--export {"saveMRDIJSON"}
needsPackage "JSON"

-- serializable types
-- {type, name of type in file, uses Id, uses parameters}
typeInfo = {
    {PolynomialRing, "MPolyRing", true, false}
    }

-- TypeMap = new HashTable {
--     PolynomialRing => "MPolyRing",
--     
-- }

typeParams = method()
typeParams PolynomialRing := (obj) -> (
    return (PolynomialRing, baseRing(obj))
    )

typeParams Ring := (obj) -> (
    if hash(obj) == hash(QQ) then return (Ring, null);
    if hash(obj) == hash(ZZ) then return (Ring, null);
    return (Ring, baseRing(obj));
    )

typeParams RingElement := (obj) -> (
    return (RingElement, ring(obj));
    );


saveTypeParams = method()
saveTypeParams(MutableHashTable, PolynomialRing) := (state,  obj) -> (
    "MPolyRing"
    );

saveTypeParams(MutableHashTable, RingElement) := (state,  obj) -> (
    new HashTable from {
	"name" => "MPolyRingElem",
	"params" => hash ring obj
	}
    );

saveObject = method()
saveObject(MutableHashTable, PolynomialRing) := (state, obj) -> (
    new HashTable from {
	"base_ring" => new HashTable from {"_type" => "QQField"},
	"symbols" => toString \ gens(obj)
	}
    )

saveObject(MutableHashTable, RingElement) := (state, obj) -> (
    listForm obj 
)


saveTypedObject = method()
saveTypedObject(MutableHashTable, Thing) := (state, obj) -> (
    state#"objToId"#(class obj) ??= hash class obj;
    state#"idToObj"#(hash class obj) ??= class obj;
    
    h := new HashTable from {
	"_type" => saveTypeParams(state, obj),
	"data" => saveObject(state, obj)
	};
	
    )

saveMRDIJSON = method()
saveMRDIJSON(String, Thing) := (filename, obj) -> (
    -- this should become a global serializer state to save many objects in the
    -- same session
    state := new MutableHashTable from {
	"objToId" => new MutableHashTable,
	"idToObj" => new MutableHashTable
	};

    objAsDict := saveTypedObject(state, obj)
    
    -- write objAsDict to a file
    );

end--
restart
needs "MRDIJSON.m2"

R = QQ[x, y]
p = x^2 + y
saveMRDIJSON("balh", 2 * p)




