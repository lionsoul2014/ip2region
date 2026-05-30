package xdb

import (
	"fmt"
	"strings"
	"sync"
)

// region manager with:
// 1, content cache.
// 2, util functions

// --- region

type Region struct {
	Str    string   // region string
	fields []string // region fields
}

var EmptyRegion = NewRegion("")

// Create a new region without checking cache info
func NewRegion(str string) *Region {
	return &Region{
		Str:    str,
		fields: nil,
	}
}

func (r *Region) Fields() []string {
	return r.SepFields("|")
}

func (r *Region) SepFields(sep string) []string {
	if r.fields == nil {
		r.fields = strings.Split(r.Str, sep)
	}

	return r.fields
}

func (r *Region) Join(sep string) string {
	if sep == "|" {
		return r.Str
	}

	return strings.Join(r.Fields(), sep)
}

func (r *Region) Filtering(fields []int) (*Region, error) {
	if len(fields) == 0 {
		return r, nil
	}

	fs := r.Fields()
	var sb []string
	for _, idx := range fields {
		if idx < 0 {
			return r, fmt.Errorf("negative filter index %d", idx)
		}

		if idx >= len(fs) {
			return r, fmt.Errorf("field index %d exceeded the max length of %d", idx, len(fs))
		}

		sb = append(sb, fs[idx])
	}

	return &Region{
		Str:    strings.Join(sb, "|"),
		fields: sb,
	}, nil
}

// Equal check ptr (share the same region cache) or the Str is the same.
func (r *Region) Equal(dst *Region) bool {
	return (r == dst || r.Str == dst.Str)
}

func (r *Region) IsEmpty() bool {
	return r.Str == ""
}

func (r *Region) String() string {
	return r.Str
}

// ---
// --- region cache

type RegionCache struct {
	lock  sync.Mutex
	cache map[string]*Region
}

func NewRegionCache() *RegionCache {
	return &RegionCache{
		lock:  sync.Mutex{},
		cache: make(map[string]*Region),
	}
}

func (rc *RegionCache) Region(str string) *Region {
	rc.lock.Lock()
	defer rc.lock.Unlock()

	region, ok := rc.cache[str]
	if ok {
		return region
	}

	// cache the new region
	region = &Region{
		Str:    str,
		fields: nil,
	}

	rc.cache[str] = region
	return region
}

func (rc *RegionCache) Swap(r *Region) *Region {
	return rc.Region(r.Str)
}

func (rc *RegionCache) Clean() {
	rc.lock.Lock()
	defer rc.lock.Unlock()

	for k := range rc.cache {
		delete(rc.cache, k)
	}
}
