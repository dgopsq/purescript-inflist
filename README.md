# ♾️ inflist

Inflist is a infinitely nestable todo list written in [PureScript](https://www.purescript.org/). It's an **excercise** and an **experiment** to better understand the interoperability between **PureScript** and [React](https://reactjs.org/).

> **N.B:** I'm still learning PureScript so *there will be* errors 🤓.
> Any correction / improvement is *highly* appreciated!

## Introduction

### Architecture

This application uses [purescript-react-basic](https://github.com/lumihq/purescript-react-basic) and [purescript-react-basic-hook](https://github.com/megamaddu/purescript-react-basic-hooks) to operate using React as the user interface library. Even though _purescript-react-basic_ is almost not maintained anymore, _purescript-react-basic-hook_ is quite a formidable porting with a really flexible API.

The global state is managed through the use of the [`useContextSelector` hook](https://github.com/dai-shi/use-context-selector) and [`useReducer` hook](https://reactjs.org/docs/hooks-reference.html#usereducer). The React [Context API](https://reactjs.org/docs/context.html) should not be used for highly dynamic values such as the application's state since it will cause the whole three to re-render everytime. The `useContextSelector` hook behave a bit more like the `useSelector` from Redux, subscribing to only the piece of context we really want to observe.

The **Storage Layer** uses the [Local Storage](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage) to save the application's current state. Even though it's possible to implement different [storage strategies](https://github.com/dgopsq/inflist/blob/master/src/App/Api/Storage/Storage.purs), the persistence is **not** optimized and the consistency is not really assured. The logic that inflist uses to store and retrieve the persisted state is a bit scattered across the application (mostly in the [`ConnectedTodo`](https://github.com/dgopsq/inflist/blob/master/src/App/Components/ConnectedTodo.purs) and [`TodosListPage`](https://github.com/dgopsq/inflist/blob/master/src/App/Pages/TodosListPage.purs) components) and too much coupled with the React lifecycle events.

### Tests

Inflist is definitely _not tested enough_. It uses [purescript-spec](https://github.com/purescript-spec/purescript-spec) as the main testing framework for unit tests. Only the reducers and the routes utilities are (a bit 😔) tested, in the future should be explored the possibility to integrate technologies like [Test Renderer](https://reactjs.org/docs/test-renderer.html) for the React components.

### Design

All the styles are managed through [Tailwind CSS](https://tailwindcss.com/) and the icons are from [css.gg](https://css.gg/).

## Development

To start making changes to this code, **PureScript** with [Spago](https://github.com/purescript/spago) is needed, along with [Yarn](https://yarnpkg.com/) to manage the JavaScript dependencies.

Follow these steps to have a development server running in local:

```bash
# Clone this repository
git clone git@github.com:dgopsq/inflist.git && cd inflist

# Install dependencies
yarn

# Build the PureScript code
spago build

# Start the development server
yarn dev
```

To build inflist for production:

```bash
yarn build
```
