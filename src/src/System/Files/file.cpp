
/******************************************************************************
* MODULE     : file.cpp
* DESCRIPTION: file handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "file.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"
#include "timer.hpp"
#include "merge_sort.hpp"
#include "data_cache.hpp"
#include "web_files.hpp"
#include "scheme.hpp"
#include "convert.hpp"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <dirent.h>
#ifdef OS_WIN32
#include <sys/misc.h>
#include <sys/_stat.h>
#include <X11/Xlib.h>
#else
#include <sys/stat.h>
#endif
#include <sys/types.h>
#include <string.h>  // strerror

#ifdef MACOSX_EXTENSIONS
#include "MacOS/mac_images.h"
#endif

/******************************************************************************
* New style loading and saving
******************************************************************************/

bool
load_string (url u, string& s, bool fatal) {
  // cout << "Load " << u << LF;
  url r= u;
  if (!is_rooted_name (r)) r= resolve (r);
  // cout << "Resolved " << r << LF;
  bool err= !is_rooted_name (r);
  if (!err) {
    string name= concretize (r);
    // cout << "Concrete :" << name << LF;
    // File contents in cache?
    bool file_flag= do_cache_file (name);
    bool doc_flag= do_cache_doc (name);
    string cache_type= doc_flag? string ("doc_cache"): string ("file_cache");
    if (doc_flag) cache_load ("doc_cache");
    if (is_cached (cache_type, name) && is_up_to_date (url_parent (r))) {
      s= cache_get (cache_type, name) -> label;
      return false;
    }
    // End caching

    bench_start ("load file");
    c_string _name (name);
    // cout << "OPEN :" << _name << LF;
#if defined (OS_WIN32)
    FILE* fin= _fopen (_name, "rb");
#elif defined (__MINGW__) || defined (__MINGW32__)
    FILE* fin= fopen (_name, "rb");
#else
    FILE* fin= fopen (_name, "r");
#endif
    if (fin == NULL) {
      err= true;
      if (!occurs ("system", name))
        std_warning << "Load error for " << name << ", "
                    << strerror(errno) << "\n";
    }
    int size= 0;
    if (!err) {
      if (fseek (fin, 0L, SEEK_END) < 0) err= true;
      else {
	size= ftell (fin);
	if (size<0) err= true;
      }
      if (err) {
        std_warning << "Seek failed for " << as_string (u) << "\n";
        fclose (fin);
      }
    }
    if (!err) {
      rewind (fin);
      s->resize (size);
      int read= fread (&(s[0]), 1, size, fin);
      if (read < size) s->resize (read);
      fclose (fin);
    }
    bench_cumul ("load file");

    // Cache file contents
    if (!err && N(s) <= 10000)
      if (file_flag || doc_flag)
	cache_set (cache_type, name, s);
    // End caching
  }
  if (err && fatal) {
    failed_error << "File name= " << as_string (u) << "\n";
    FAILED ("file not readable");
  }
  return err;
}

bool
save_string (url u, string s, bool fatal) {
  if (is_rooted_tmfs (u)) {
    bool err= save_to_server (u, s);
    if (err && fatal) {
      failed_error << "File name= " << as_string (u) << "\n";
      FAILED ("file not writeable");
    }
    return err;
  }

  // cout << "Save " << u << LF;
  url r= u;
  if (!is_rooted_name (r)) r= resolve (r, "");
  bool err= !is_rooted_name (r);
  if (!err) {
    string name= concretize (r);
    {
      c_string _name (name);
#if defined (OS_WIN32)
      FILE* fout= _fopen (_name, "wb");
#elif defined (__MINGW__) || defined (__MINGW32__)
      FILE* fout= fopen (_name, "wb");
#else
      FILE* fout= fopen (_name, "w");
#endif
      if (fout == NULL) {
        err= true;
        std_warning << "Save error for " << name << ", "
                    << strerror(errno) << "\n";
      }
      if (!err) {
        int i, n= N(s);
        for (i=0; i<n; i++)
          fputc (s[i], fout);
        fclose (fout);
      }
    }
    // Cache file contents
    bool file_flag= do_cache_file (name);
    bool doc_flag= do_cache_doc (name);
    string cache_type= doc_flag? string ("doc_cache"): string ("file_cache");
    if (!err && N(s) <= 10000)
      if (file_flag || doc_flag)
	cache_set (cache_type, name, s);
    declare_out_of_date (url_parent (r));
    // End caching
  }

  if (err && fatal) {
    failed_error << "File name= " << as_string (u) << "\n";
    FAILED ("file not writeable");
  }
  return err;
}

bool
append_string (url u, string s, bool fatal) {
  if (is_rooted_tmfs (u)) FAILED ("file not appendable");

  // cout << "Save " << u << LF;
  url r= u;
  if (!is_rooted_name (r)) r= resolve (r, "");
  bool err= !is_rooted_name (r);
  if (!err) {
    string name= concretize (r);
    {
      c_string _name (name);
#if defined (OS_WIN32)
      FILE* fout= _fopen (_name, "ab");
#elif defined (__MINGW__) || defined (__MINGW32__)
      FILE* fout= fopen (_name, "ab");
#else
      FILE* fout= fopen (_name, "a");
#endif
      if (fout == NULL) {
        err= true;
        std_warning << "Append error for " << name << ", "
                    << strerror(errno) << "\n";
      }
      if (!err) {
        int i, n= N(s);
        for (i=0; i<n; i++)
          fputc (s[i], fout);
        fclose (fout);
      }
    }
    // Cache file contents
    declare_out_of_date (url_parent (r));
    // End caching
  }

  if (err && fatal) {
    failed_error << "File name= " << as_string (u) << "\n";
    FAILED ("file not appendable");
  }
  return err;
}

/******************************************************************************
* Getting attributes of a file
******************************************************************************/

static bool
get_attributes (url name, struct stat* buf,
		bool link_flag=false, bool cache_flag= true)
{
  // cout << "Stat " << name << LF;
  string name_s= concretize (name);

  // Stat result in cache?
  if (cache_flag &&
      is_cached ("stat_cache.scm", name_s) &&
      is_up_to_date (url_parent (name)))
    {
      tree r= cache_get ("stat_cache.scm", name_s);
      // cout << "Cache : " << r << LF;
      if (r == "#f") return true;
      if ((is_compound(r)) && (N(r)==2)) {
        buf->st_mode = ((unsigned int) as_int (r[0]));
        buf->st_mtime= ((unsigned int) as_int (r[1]));
        return false;
      } 
      std_warning << "Inconsistent value in stat_cache.scm for key "
                  << name_s << LF;
      std_warning << "The current value is " << r << LF;
      std_warning << "I'm resetting this key" << LF;
      // continue and recache, the current value is inconsistent. 
    }
  // End caching

  //cout << "No cache" << LF;

  bench_start ("stat");
  bool flag;
  c_string temp (name_s);
#ifdef OS_WIN32
  flag= _stat (temp, buf);
#else
  flag= stat (temp, buf);
#endif
  (void) link_flag;
  // FIXME: configure should test whether lstat works
  // flag= (link_flag? lstat (temp, buf): stat (temp, buf));
  bench_cumul ("stat");

  // Cache stat results
  if (cache_flag) {
    if (flag) {
      if (do_cache_stat_fail (name_s))
	cache_set ("stat_cache.scm", name_s, "#f");
    }
    else {
      if (do_cache_stat (name_s)) {
        string s1= as_string ((int) buf->st_mode);
        string s2= as_string ((int) buf->st_mtime);
	cache_set ("stat_cache.scm", name_s, tree (TUPLE, s1, s2));
      }
    }
  }
  // End caching

  return flag;
}

bool
is_of_type (url name, string filter) {
  if (filter == "") return true;
  int i, n= N(filter);

  // Files from the web
  if (is_rooted_web (name)) {
    // cout << "  try " << name << "\n";
    url from_web= get_from_web (name);
    // cout << "  --> " << from_web << "\n";
    if (is_none (from_web)) return false;
    for (i=0; i<n; i++)
      switch (filter[i]) {
      case 'd': return false;
      case 'l': return false;
      case 'w': return false;
      case 'x': return false;
      }
    return true;
  }

  // Files from a remote server
  if (is_rooted_tmfs (name)) {
    for (i=0; i<n; i++)
      switch (filter[i]) {
      case 'd': return false;
      case 'l': return false;
      case 'r':
	if (!as_bool (call ("tmfs-permission?", name, "read")))
	  return false;
	break;
      case 'w':
	if (!as_bool (call ("tmfs-permission?", name, "write")))
	  return false;
	break;
      case 'x': return false;
      }
    return true;
  }

  // Files from the ramdisk
  if (is_ramdisc (name))
    return true;

  // Normal files
#if defined (OS_WIN32) || defined (__MINGW__) || defined (__MINGW32__)
  if ((filter == "x") && (suffix(name) != "exe") && (suffix(name) != "bat"))
    name = glue (name, ".exe");
#endif
  bool preserve_links= false;
  for (i=0; i<n; i++)
    preserve_links= preserve_links || (filter[i] == 'l');
  struct stat buf;
  bool err= get_attributes (name, &buf, preserve_links);
  for (i=0; i<n; i++)
    switch (filter[i]) {
      // FIXME: should check user id and group id for r, w and x
    case 'f':
      if (err || !S_ISREG (buf.st_mode)) return false;
      break;
    case 'd':
      if (err || !S_ISDIR (buf.st_mode)) return false;
      break;
    case 'l':
#ifdef __MINGW32__
      return false;
#else
      if (err || !S_ISLNK (buf.st_mode)) return false;
#endif
      break;
    case 'r':
      if (err) return false;
#ifndef __MINGW32__
      if ((buf.st_mode & (S_IRUSR | S_IRGRP | S_IROTH)) == 0) return false;
#else
      if ((buf.st_mode & 292) == 0) return false;
#endif
      break;
    case 'w':
      if (err) return false;
#ifndef __MINGW32__
      if ((buf.st_mode & (S_IWUSR | S_IWGRP | S_IWOTH)) == 0) return false;
#else
      if ((buf.st_mode & 146) == 0) return false;
#endif
      break;
    case 'x':
      if (err) return false;
#if defined (OS_WIN32) || defined (__MINGW__) || defined (__MINGW32__)
      if (suffix(name) == "bat") break;
#endif
#ifndef __MINGW32__
      if ((buf.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH)) == 0) return false;
#else
      if ((buf.st_mode & 73) == 0) return false;
#endif
      break;
    }
  return true;
}

bool is_regular (url name) { return is_of_type (name, "f"); }
bool is_directory (url name) { return is_of_type (name, "d"); }
bool is_symbolic_link (url name) { return is_of_type (name, "l"); }

int
last_modified (url u, bool cache_flag) {
  if (is_rooted_web (u))
    return - (int) (((unsigned int) (-1)) >> 1);
  if (is_rooted_tmfs (u))
    return - (int) (((unsigned int) (-1)) >> 1);
  struct stat u_stat;
  if (get_attributes (u, &u_stat, true, cache_flag))
    return - (int) (((unsigned int) (-1)) >> 1);
  return u_stat.st_mtime;
}

bool
is_newer (url which, url than) {
  struct stat which_stat;
  struct stat than_stat;
  // FIXME: why was this? 
  if (is_cached ("stat_cache.scm", concretize (which))) return false;
  if (is_cached ("stat_cache.scm", concretize (than))) return false;
  // end FIXME
  if (get_attributes (which, &which_stat, true)) return false;
  if (get_attributes (than , &than_stat , true)) return false;
  return which_stat.st_mtime > than_stat.st_mtime;
}

url
url_temp (string suffix) {
#ifdef __MINGW32__
  unsigned int rnd= raw_time ();
#else
  static bool initialized= false;
  if (!initialized) {
    srandom ((int) raw_time ());
    initialized= true;
  }
  unsigned int rnd= random ();
#endif
  string name= "tmp_" * as_string (rnd) * suffix;
  url u= url_temp_dir () * url (name);
  if (exists (u)) return url_temp (suffix);
  return u;
}

url
url_numbered (url dir, string prefix, string postfix, int i) {
  if (!exists (dir)) mkdir (dir);
  for (; true; i++) {
    url name= dir * (prefix * as_string (i) * postfix);
    if (!exists (name)) return name;
  }
  return dir * (prefix * "x" * postfix);
}

url
url_scratch (string prefix, string postfix, int i) {
  url dir ("$TEXMACS_HOME_PATH/texts/scratch");
  return url_numbered (dir, prefix, postfix, i);
}

bool
is_scratch (url u) {
  return head (u) == url ("$TEXMACS_HOME_PATH/texts/scratch");
}

string
file_format (url u) {
  if (is_rooted_tmfs (u))
    return as_string (call ("tmfs-format", object (u)));
  else return suffix_to_format (suffix (u));
}

/******************************************************************************
* Reading directories
******************************************************************************/

static array<string>
cache_dir_get (string dir) {
  tree t= cache_get ("dir_cache.scm", dir);
  array<string> a (N(t));
  for (int i=0; i<N(t); i++) a[i]= t[i]->label;
  return a;
}

static void
cache_dir_set (string dir, array<string> a) {
  tree t (TUPLE, N(a));
  for (int i=0; i<N(a); i++) t[i]= a[i];
  cache_set ("dir_cache.scm", dir, t);
}

array<string>
read_directory (url u, bool& error_flag) {
  // cout << "Directory " << u << LF;
  u= resolve (u, "dr");
  if (is_none (u)) return array<string> ();
  string name= concretize (u);

  // Directory contents in cache?
  if (is_cached ("dir_cache.scm", name) && is_up_to_date (u))
    return cache_dir_get (name);
  bench_start ("read directory");
  // End caching

  DIR* dp;
  c_string temp (name);
  dp= opendir (temp);
  error_flag= (dp==NULL);
  if (error_flag) return array<string> ();

  array<string> dir;
  struct dirent* ep;
  while (true) {
    ep= readdir (dp);
    if (ep==NULL) break;
    dir << string (ep->d_name);
  }
  (void) closedir (dp);
  merge_sort (dir);

  // Caching of directory contents
  bench_cumul ("read directory");
  if (do_cache_dir (name))
    cache_dir_set (name, dir);
  // End caching

  return dir;
}

/******************************************************************************
* Commands on files
******************************************************************************/

void
move (url u1, url u2) {
  c_string _u1 (concretize (u1));
  c_string _u2 (concretize (u2));
  (void) rename (_u1, _u2);
}

void
copy (url u1, url u2) {
  string s;
  if (!load_string (u1, s, false))
    (void) save_string (u2, s, false);
}

void
remove_sub (url u) {
  if (is_none (u));
  else if (is_or (u)) {
    remove_sub (u[1]);
    remove_sub (u[2]);
  }
  else {
    c_string _u (concretize (u));
    if (::remove (_u) && DEBUG_AUTO) {
      std_warning << "Remove failed: " << strerror (errno) << LF;
      std_warning << "File was: " << u << LF;
    }
  }
}

void
remove (url u) {
  remove_sub (expand (complete (u)));
}

void
append_to (url what, url to) {
  string what_s;
  if (load_string (what, what_s, false) ||
      append_string (to, what_s, false))
    std_warning << "Append failed for " << to << LF;
}

void
rmdir (url u) {
  remove_sub (expand (complete (u, "dr")));
}

void
mkdir (url u) {
#if defined (HAVE_SYS_TYPES_H) && defined (HAVE_SYS_STAT_H)
  if (exists (u)) return;
  {
    c_string _u (concretize (u));
#if defined(__MINGW__) || defined(__MINGW32__)
    (void) ::mkdir (_u);
#else
    (void) ::mkdir (_u, S_IRWXU + S_IRGRP + S_IROTH);
#endif
  }
#else
#if defined(__MINGW__) || defined(__MINGW32__)
  system ("mkdir", u);
#else
  system ("mkdir -p", u);
#endif
#endif
}

void
change_mode (url u, int mode) {
#if defined (HAVE_SYS_TYPES_H) && defined (HAVE_SYS_STAT_H)
  c_string _u (concretize (u));
  (void) ::chmod (_u, mode);
#else
  string m0= as_string ((mode >> 9) & 7);
  string m1= as_string ((mode >> 6) & 7);
  string m2= as_string ((mode >> 3) & 7);
  string m3= as_string (mode & 7);
  system ("chmod -f " * m0 * m1 * m2 * m3, u);
#endif
}

void
ps2pdf (url u1, url u2) {
#ifdef OS_WIN32
  c_string _u1 (concretize (u1));
  c_string _u2 (concretize (u2));
  XPs2Pdf (_u1, _u2);
#else
#ifdef MACOSX_EXTENSIONS
  mac_ps_to_pdf (u1, u2);
#else
  system ("ps2pdf", u1, u2);
#endif
#endif
}

/******************************************************************************
* Tab-completion for file names
******************************************************************************/

#ifdef OS_WIN32
#define URL_CONCATER  '\\'
#else
#define URL_CONCATER  '/'
#endif

static void
file_completions_file (array<string>& a, url search, url u) {
  if (is_or (u)) {
    file_completions_file (a, search, u[1]);
    file_completions_file (a, search, u[2]);
  }
  else {
    url v= delta (search * url ("dummy"), u);
    if (is_none (v)) return;
    string s= as_string (v);
    if (is_directory (u)) s= s * string (URL_CONCATER);
    a << s;
  }
}

static void
file_completions_dir (array<string>& a, url search, url dir) {
  if (is_or (search)) {
    file_completions_dir (a, search[1], dir);
    file_completions_dir (a, search[2], dir);
  }
  else if (is_or (dir)) {
    file_completions_dir (a, search, dir[1]);
    file_completions_dir (a, search, dir[2]);
  }
  else {
    url u= search * dir * url_wildcard ("*");
    u= complete (u, "r");
    u= expand (u);
    file_completions_file (a, search, u);
  }
}

array<string>
file_completions (url search, url dir) {
  array<string> a;
  file_completions_dir (a, search, dir);
  return a;
}

/******************************************************************************
* Grepping of strings with heavy caching
******************************************************************************/

hashmap<tree,tree>   grep_cache (url_none () -> t);
hashmap<tree,string> grep_load_cache ("");
hashmap<tree,tree>   grep_complete_cache (url_none () -> t);

static bool
bad_url (url u) {
  if (is_atomic (u))
    return u == url ("aapi") || u == url (".svn");
  else if (is_concat (u))
    return bad_url (u[1]) || bad_url (u[2]);
  else return false;
}

string
grep_load (url u) {
  if (!grep_load_cache->contains (u->t)) {
    //cout << "Loading " << u << "\n";
    string s;
    if (load_string (u, s, false)) s= "";
    grep_load_cache (u->t)= s;
  }
  return grep_load_cache [u->t];
}

url
grep_sub (string what, url u) {
  if (is_or (u))
    return grep_sub (what, u[1]) | grep_sub (what, u[2]);
  else if (bad_url (u))
    return url_none ();
  else {
    string contents= grep_load (u);
    if (occurs (what, contents)) return u;
    else return url_none ();
  }
}

url
grep (string what, url u) {
  tree key= tuple (what, u->t);
  if (!grep_cache->contains (key)) {
    if (!grep_complete_cache->contains (u->t))
      grep_complete_cache (u->t)= expand (complete (u)) -> t;
    url found= grep_sub (what, as_url (grep_complete_cache [u->t]));
    grep_cache (key)= found->t;
  }
  return as_url (grep_cache [key]);
}

/******************************************************************************
* Searching text in the documentation
******************************************************************************/

static array<int>
search (string what, string in) {
  int i= 0, n= N(what);
  array<int> matches;
  if (n == 0) return matches;
  while (true) {
    int pos= search_forwards (what, i, in);
    if (pos == -1) return matches;
    matches << pos;
    i= pos+1;
  }
}

static bool
precedes (string in, int pos, string what) {
  return pos >= N(what) && in (pos-N(what), pos) == what;
}

static int
compute_score (string what, string in, int pos, string suf) {
  int score= 1;
  if (pos > 0 && !is_iso_alpha (in [pos-1]))
    if (pos + N(what) + 1 < N(in) && !is_iso_alpha (in [pos+N(what)]))
      score *= 10;
  if (suf == "tm") {
    if (precedes (in, pos, "<")) score= 0;
    else if (precedes (in, pos, "<\\")) score= 0;
    else if (precedes (in, pos, "<|")) score= 0;
    else if (precedes (in, pos, "</")) score= 0;
    else if (precedes (in, pos, "compound|")) score= 0;
    else if (precedes (in, pos, "<name|")) score *= 10;
    else if (precedes (in, pos, "<tmstyle|")) score *= 10;
    else if (precedes (in, pos, "<tmdtd|")) score *= 10;
    else if (precedes (in, pos, "<explain-macro|")) score *= 10;
    else if (precedes (in, pos, "<var-val|")) score *= 10;
  }
  else if (suf == "scm") {
    if (precedes (in, pos, "define ")) score *= 10;
    else if (precedes (in, pos, "define-public ")) score *= 10;
    else if (precedes (in, pos, "define (")) score *= 10;
    else if (precedes (in, pos, "define-public (")) score *= 10;
    else if (precedes (in, pos, "define-macro ")) score *= 10;
    else if (precedes (in, pos, "define-public-macro ")) score *= 10;
    else if (precedes (in, pos, "define-macro (")) score *= 10;
    else if (precedes (in, pos, "define-public-macro (")) score *= 10;
  }
  return score;
}

static int
compute_score (string what, string in, array<int> pos, string suf) {
  int score= 0, i= 0, n= N(pos);
  for (i=0; i<n; i++)
    score += compute_score (what, in, pos[i], suf);
  return score;
}

int
search_score (url u, array<string> a) {
  string in = grep_load (u);
  if (N(in) == 0) return 0;
  in= locase_all (in);
  string suf= suffix (u);
  int i, score= 1, n= N(a);
  for (i=0; i<n; i++) {
    string what= locase_all (a[i]);
    array<int> pos= search (what, in);
    score *= compute_score (what, in, pos, suf);
    if (score == 0) return 0;
    if (score > 1000000) score= 1000000;
  }
  return score;
}
