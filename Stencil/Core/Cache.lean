/-
  Stencil.Core.Cache
  Template caching for faster repeated renders
-/
import Stencil.AST.Types
import Stencil.Core.Error
import Std.Data.HashMap

namespace Stencil

/-- Template cache entry -/
structure CacheEntry where
  template : Template
  hash : UInt64
  deriving Inhabited

/-- Template cache (per-engine instance) -/
structure TemplateCache where
  entries : Std.HashMap UInt64 CacheEntry
  maxSize : Nat := 1000
  deriving Inhabited

namespace TemplateCache

/-- Create empty cache -/
def empty : TemplateCache := ⟨{}, 1000⟩

/-- Create cache with custom max size -/
def withMaxSize (maxSize : Nat) : TemplateCache := ⟨{}, maxSize⟩

/-- Get cached template by source string -/
def get? (cache : TemplateCache) (source : String) : Option Template :=
  let hash := source.hash
  cache.entries.get? hash |>.map (·.template)

/-- Add template to cache -/
def put (cache : TemplateCache) (source : String) (tmpl : Template) : TemplateCache :=
  let hash := source.hash
  -- Simple eviction: if at max size, just don't add
  -- (could implement LRU later)
  if cache.entries.size >= cache.maxSize then
    cache
  else
    { cache with entries := cache.entries.insert hash ⟨tmpl, hash⟩ }

/-- Clear the cache -/
def clear (cache : TemplateCache) : TemplateCache :=
  { cache with entries := {} }

/-- Get cache size -/
def size (cache : TemplateCache) : Nat :=
  cache.entries.size

end TemplateCache

end Stencil
