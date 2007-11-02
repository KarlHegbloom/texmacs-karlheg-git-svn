
/******************************************************************************
* MODULE     : env.cpp
* DESCRIPTION: the environment of the math_editor
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "env.hpp"
#include "iterator.hpp"
extern hashmap<string,int> default_var_type;
void initialize_default_var_type ();
extern hashmap<string,tree> default_env;
void initialize_default_env ();
#include "page_type.hpp"

/******************************************************************************
* Initialization
******************************************************************************/

edit_env_rep::edit_env_rep (drd_info& drd2,
			    url base_file_name2,
			    hashmap<string,tree>& local_ref2,
			    hashmap<string,tree>& global_ref2,
			    hashmap<string,tree>& local_aux2,
			    hashmap<string,tree>& global_aux2):
  drd (drd2),
  env (UNINIT), back (UNINIT), src (path (DECORATION)),
  var_type (default_var_type),
  base_file_name (base_file_name2),
  cur_file_name (base_file_name2),
  secure (is_secure (base_file_name2)),
  local_ref (local_ref2), global_ref (global_ref2),
  local_aux (local_aux2), global_aux (global_aux2)
{
  initialize_default_env ();
  initialize_default_var_type ();
  env= copy (default_env);
  style_init_env ();
  update ();
  complete= false;
  recover_env= tuple ();
}

edit_env::edit_env (drd_info& drd,
		    url base_file_name,
		    hashmap<string,tree>& local_ref,
		    hashmap<string,tree>& global_ref,
		    hashmap<string,tree>& local_aux,
		    hashmap<string,tree>& global_aux):
  rep (new edit_env_rep (drd, base_file_name,
			 local_ref, global_ref, local_aux, global_aux)) {}

void
edit_env_rep::style_init_env () {
  dpi = get_int (DPI);
  inch= ((double) dpi*PIXEL);
  flexibility= get_double (PAGE_FLEXIBILITY);
  back= hashmap<string,tree> (UNINIT);
  update_page_pars ();
}

/******************************************************************************
* Modification of environment variables
******************************************************************************/

/*
void
edit_env_rep::set (string s, tree t) {
  // cout << "Set " << s << " := " << t << "\n";
  tree r= exec (t); // Replacing r by exec(t) below yields bug when optimizing
  // cout << "Do " << s << " := " << r << "\n";
  monitored_write (s, tree (BACKUP, r, read(s)));
  update (s);
}

void
edit_env_rep::reset (string s) {
  // cout << "Reset " << s << "\n";
  if (is_func (read(s), BACKUP, 2)) monitored_write (s, read(s)[1]);
  update (s);
}

void
edit_env_rep::assign (string s, tree t) {
  // cout << "Assign " << s << " := " << t << "\n";
  t= exec (t);
  // cout << "Do     " << s << " := " << t << "\n";
  if (is_func (read(s), BACKUP, 2)) {
    if (read(s)[0] != t) {
      monitored_write (s, tree (BACKUP, t, read(s)[1]));
      update (s);
    }
  }
  else {
    if (read(s) != t) {
      monitored_write (s, t);
      update (s);
    }
  }
}
*/

/******************************************************************************
* Global manipulations of the environment
******************************************************************************/

void
edit_env_rep::write_default_env () {
  env= copy (default_env);
}

void
edit_env_rep::write_env (hashmap<string,tree> user_env) {
  env= copy (user_env);
}

void
edit_env_rep::monitored_patch_env (hashmap<string,tree> patch) {
  if (patch->size == 0) return;
  int i=0, n=patch->n;
  for (; i<n; i++) {
    list<hashentry<string,tree> > l=patch->a[i];
    for (; !nil(l); l=l->next)
      monitored_write_update (l->item.key, l->item.im);
  }
}

void
edit_env_rep::patch_env (hashmap<string,tree> patch) {
  if (patch->size == 0) return;
  int i=0, n=patch->n;
  for (; i<n; i++) {
    list<hashentry<string,tree> > l=patch->a[i];
    for (; !nil(l); l=l->next)
      write_update (l->item.key, l->item.im);
  }
}

void
edit_env_rep::read_env (hashmap<string,tree>& ret) {
  ret= copy (env);
}

void
edit_env_rep::local_start (hashmap<string,tree>& prev_back) {
  prev_back= back;
  back= hashmap<string,tree> (UNINIT);  
}

void
edit_env_rep::local_update (hashmap<string,tree>& old_patch,
			    hashmap<string,tree>& change)
{
  old_patch->pre_patch (back, env);
  old_patch->post_patch (change, env);
  change= invert (back, env);
}

void
edit_env_rep::local_end (hashmap<string,tree>& prev_back) {
  int i=0, n=back->n;
  for (; i<n; i++) {
    list<hashentry<string,tree> > l=back->a[i];
    for (; !nil(l); l=l->next)
      prev_back->write_back (l->item.key, back);
  }
  back= prev_back;
}

ostream&
operator << (ostream& out, edit_env env) {
  return out << env->env;
}
