switch ReactDOM.querySelector("#app") {
| Some(root) => ReactDOM.render(<App />, root)
| None => failwith("Wrong DOM id of the root element")
}
