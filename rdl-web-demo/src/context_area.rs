use mogwai::prelude::*;
use rdl_lib::runtime::Context;

pub(crate) struct ContextArea {
    initial_context: Context,
    current_context: Context,
    show_full: bool,
}

#[derive(Clone)]
pub(crate) enum ContextAreaIn {
    ShowFullContext(bool),
    NewContext(Context),
    ResetContext,
}

#[derive(Clone)]
pub(crate) enum ContextAreaOut {
    PatchContext(Patch<View<HtmlElement>>, bool),
}

impl Default for ContextArea {
    fn default() -> Self {
        ContextArea {
            initial_context: Context::get_global_context(),
            current_context: Context::get_global_context(),
            show_full: true,
        }
    }
}

impl ContextArea {
    fn send_context_update(&self, tx: &Transmitter<ContextAreaOut>) {
        let get_patch_context = |k, v| -> ContextAreaOut {
            ContextAreaOut::PatchContext(
                Patch::PushBack {
                    value: view! {
                    <span>
                        <code>{format!("{}:", k)}</code>
                        <code style="float: right; overflow-wrap: anywhere;">{format!("{:?}", v)}</code><br/>
                    </span>},
                },
                self.show_full,
            )
        };
        tx.send(&ContextAreaOut::PatchContext(
            Patch::RemoveAll,
            self.show_full,
        ));
        if self.show_full {
            for (k, v) in &self.current_context.context {
                tx.send(&get_patch_context(k, v));
            }
        } else {
            for (k, v) in &self
                .current_context
                .context
                .clone()
                .symmetric_difference_with(self.initial_context.context.clone(), |cur, init| {
                    if cur != init {
                        Some(cur)
                    } else {
                        None
                    }
                })
            {
                tx.send(&get_patch_context(k, v));
            }
        }
    }
}

impl Component for ContextArea {
    type DomNode = HtmlElement;
    type ModelMsg = ContextAreaIn;
    type ViewMsg = ContextAreaOut;

    fn update(
        &mut self,
        msg: &ContextAreaIn,
        tx: &Transmitter<ContextAreaOut>,
        _sub: &Subscriber<ContextAreaIn>,
    ) {
        match msg {
            ContextAreaIn::ShowFullContext(show_full) => {
                if self.show_full != *show_full {
                    self.show_full = *show_full;
                    self.send_context_update(tx);
                }
            }
            ContextAreaIn::NewContext(context) => {
                if context.context != self.current_context.context {
                    self.current_context = context.clone();
                    self.send_context_update(tx);
                }
            }
            ContextAreaIn::ResetContext => {
                self.current_context = self.initial_context.clone();
                Context::set_global_context(self.current_context.clone());
                self.send_context_update(tx);
            }
        }
    }

    fn view(
        &self,
        tx: &Transmitter<ContextAreaIn>,
        rx: &Receiver<ContextAreaOut>,
    ) -> ViewBuilder<HtmlElement> {
        builder! {
            <fieldset style="height: 100%; display:flex; flex-direction:column;">
                <fieldset>
                    <legend>"Controls"</legend>
                    <button style:cursor="pointer" on:click=tx.contra_map(|_:&Event| ContextAreaIn::ResetContext)>
                            "Reset"
                    </button>
                    <form>
                        <input type="radio" id="full-context" name="context" value="full"
                            boolean:checked={(self.show_full , rx.branch_map(|ContextAreaOut::PatchContext(_,show_full)| *show_full))}
                            on:click=tx.contra_map(|_:&Event| ContextAreaIn::ShowFullContext(true))/>
                        <label for="full">"Full"</label>
                        <input type="radio" id="user-only-context" name="context" value="user-only"
                            boolean:checked={(!self.show_full , rx.branch_map(|ContextAreaOut::PatchContext(_,show_full)| !*show_full))}
                            on:click=tx.contra_map(|_:&Event| ContextAreaIn::ShowFullContext(false))/>
                        <label for="user-only">"User only "</label>
                    </form>
                </fieldset>
                <legend>"Global Context"</legend>
                <div style="overflow: auto; overflow-x:hidden;"
                    patch:children=rx.branch_map(|ContextAreaOut::PatchContext(patch,_)| patch.clone())>
                </div>
            </fieldset>
        }
    }
}
