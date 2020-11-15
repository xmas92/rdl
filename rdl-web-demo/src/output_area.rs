use mogwai::prelude::*;

pub(crate) struct OutputArea {}

#[derive(Clone)]
pub(crate) enum OutputAreaIn {
    AppendText(Vec<String>),
}

#[derive(Clone)]
pub(crate) enum OutputAreaOut {
    PatchText(Patch<View<HtmlElement>>),
}

impl OutputArea {
    fn println(&self, string: &str, tx: &Transmitter<OutputAreaOut>) {
        tx.send(&OutputAreaOut::PatchText(Patch::PushFront {
            value: view! {
            <span>
                <code style="white-space: pre-wrap;">{format!("{}",string)}</code> <br/>
            </span>},
        }));
    }
}

impl Default for OutputArea {
    fn default() -> Self {
        OutputArea {}
    }
}
impl Component for OutputArea {
    type DomNode = HtmlElement;
    type ModelMsg = OutputAreaIn;
    type ViewMsg = OutputAreaOut;

    fn update(
        &mut self,
        msg: &OutputAreaIn,
        tx: &Transmitter<OutputAreaOut>,
        _sub: &Subscriber<OutputAreaIn>,
    ) {
        match msg {
            OutputAreaIn::AppendText(strings) => {
                log::info!("Printing msg {:?}", strings);
                for s in strings.iter().rev() {
                    self.println(s, tx);
                }
            }
        }
    }

    fn view(
        &self,
        _tx: &Transmitter<OutputAreaIn>,
        rx: &Receiver<OutputAreaOut>,
    ) -> ViewBuilder<HtmlElement> {
        builder! {
            <fieldset>
                <legend>"Output"</legend>
                <div
                    patch:children=rx.branch_map(|OutputAreaOut::PatchText(patch)| patch.clone())>
                </div>
            </fieldset>
        }
    }
}
